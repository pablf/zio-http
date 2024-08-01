/*
 * Copyright 2021 - 2023 Sporta Technologies PVT LTD & the ZIO HTTP contributors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package zio.http.codec.internal

import scala.util.Try

import zio._

import zio.stream.ZStream

import zio.schema.Schema
import zio.schema.codec.BinaryCodec

import zio.http.Header.Accept.MediaTypeWithQFactor
import zio.http._
import zio.http.codec._

private[codec] trait EncoderDecoder[-AtomTypes, Value] { self =>
  def decode(url: URL, status: Status, method: Method, headers: Headers, body: Body)(implicit
    trace: Trace,
  ): Task[Value]

  def encodeWith[Z](value: Value, outputTypes: Chunk[MediaTypeWithQFactor])(
    f: (URL, Option[Status], Option[Method], Headers, Body) => Z,
  ): Z

}
private[codec] object EncoderDecoder {
  def apply[AtomTypes, Value](
    httpCodec: HttpCodec[AtomTypes, Value],
  ): EncoderDecoder[AtomTypes, Value] = {
    val flattened = httpCodec.alternatives

    flattened.length match {
      case 0 => Undefined()
      case 1 => Single(flattened.head._1)
      case _ => Multiple(flattened)
    }
  }

  private final case class Multiple[-AtomTypes, Value](
    httpCodecs: Chunk[(HttpCodec[AtomTypes, Value], HttpCodec.Fallback.Condition)],
  ) extends EncoderDecoder[AtomTypes, Value] {
    val singles = httpCodecs.map { case (httpCodec, condition) => Single(httpCodec) -> condition }

    def decode(url: URL, status: Status, method: Method, headers: Headers, body: Body)(implicit
      trace: Trace,
    ): Task[Value] = {
      def tryDecode(i: Int, lastError: Cause[Throwable]): Task[Value] = {
        if (i >= singles.length) ZIO.refailCause(lastError)
        else {
          val (codec, condition) = singles(i)

          if (condition.isMissingDataOnly && !HttpCodecError.isMissingDataOnly(lastError))
            tryDecode(i + 1, lastError)
          else
            codec
              .decode(url, status, method, headers, body)
              .catchAllCause(cause =>
                if (HttpCodecError.isHttpCodecError(cause)) {
                  tryDecode(i + 1, lastError && cause)
                } else ZIO.refailCause(cause),
              )
        }
      }

      tryDecode(0, Cause.empty)
    }

    def encodeWith[Z](value: Value, outputTypes: Chunk[MediaTypeWithQFactor])(
      f: (URL, Option[Status], Option[Method], Headers, Body) => Z,
    ): Z = {
      var i         = 0
      var encoded   = null.asInstanceOf[Z]
      var lastError = null.asInstanceOf[Throwable]

      while (i < singles.length) {
        val (current, _) = singles(i)

        try {
          encoded = current.encodeWith(value, outputTypes)(f)

          i = singles.length // break
        } catch {
          case error: HttpCodecError =>
            // TODO: Aggregate all errors in disjunction:
            lastError = error
        }

        i = i + 1
      }

      if (encoded == null) throw lastError
      else encoded
    }
  }

  private final case class Undefined[-AtomTypes, Value]() extends EncoderDecoder[AtomTypes, Value] {

    val encodeWithErrorMessage =
      """
        |Trying to encode with Undefined codec. That means that encode was invoked for object of type Nothing - which cannot exist.
        |Verify that middleware and endpoint have proper types or submit bug report at https://github.com/zio/zio-http/issues
    """.stripMargin.trim()

    val decodeErrorMessage =
      """
        |Trying to decode with Undefined codec. That means that decode was invoked for object of type Nothing - which cannot exist.
        |Verify that middleware and endpoint have proper types or submit bug report at https://github.com/zio/zio-http/issues
    """.stripMargin.trim()

    override def encodeWith[Z](
      value: Value,
      outputTypes: Chunk[MediaTypeWithQFactor],
    )(f: (zio.http.URL, Option[zio.http.Status], Option[zio.http.Method], zio.http.Headers, zio.http.Body) => Z): Z = {
      throw new IllegalStateException(encodeWithErrorMessage)
    }

    override def decode(
      url: zio.http.URL,
      status: zio.http.Status,
      method: zio.http.Method,
      headers: zio.http.Headers,
      body: zio.http.Body,
    )(implicit trace: zio.Trace): zio.Task[Value] = {
      ZIO.fail(new IllegalStateException(decodeErrorMessage))
    }
  }

  private final case class Single[-AtomTypes, Value](
    httpCodec: HttpCodec[AtomTypes, Value],
  ) extends EncoderDecoder[AtomTypes, Value] {
    private val constructor   = Mechanic.makeConstructor(httpCodec)
    private val deconstructor = Mechanic.makeDeconstructor(httpCodec)

    private val flattened: AtomizedCodecs = AtomizedCodecs.flatten(httpCodec)

    private val formFieldEncoders: Chunk[(String, Any) => FormField] =
      flattened.content.map { bodyCodec => (name: String, value: Any) =>
        {
          val (mediaType, codec) = {
            bodyCodec match {
              case BodyCodec.Single(codec, _)   =>
                codec.choices.headOption
              case BodyCodec.Multiple(codec, _) =>
                codec.choices.headOption
              case _                            =>
                None
            }
          }.getOrElse {
            throw HttpCodecError.CustomError(
              "CodecNotFound",
              s"Cannot encode multipart/form-data field $name: no codec found",
            )
          }

          if (mediaType.binary) {
            FormField.binaryField(
              name,
              codec.codec.asInstanceOf[BinaryCodec[Any]].encode(value),
              mediaType,
            )
          } else {
            FormField.textField(
              name,
              codec.codec.asInstanceOf[BinaryCodec[Any]].encode(value).asString,
              mediaType,
            )
          }
        }
      }

    implicit val trace: Trace = Trace.empty

    private val formFieldDecoders: Chunk[FormField => IO[Throwable, Any]] =
      flattened.content.map { bodyCodec => (field: FormField) =>
        {
          val mediaType = field.contentType
          val codec     = {
            bodyCodec match {
              case BodyCodec.Empty              =>
                None
              case BodyCodec.Single(codec, _)   =>
                codec.lookup(mediaType)
              case BodyCodec.Multiple(codec, _) =>
                codec.lookup(mediaType)
            }
          }.getOrElse { throw HttpCodecError.UnsupportedContentType(mediaType.fullType) }

          field.asChunk.flatMap(chunk => ZIO.fromEither(codec.codec.decode(chunk)))

        }
      }
    private val formBoundary                = Boundary("----zio-http-boundary-D4792A5C-93E0-43B5-9A1F-48E38FDE5714")
    private val indexByName                 = flattened.content.zipWithIndex.map { case (codec, idx) =>
      codec.name.getOrElse("field" + idx.toString) -> idx
    }.toMap
    private val nameByIndex                 = indexByName.map(_.swap)
    private val isByteStream                =
      if (flattened.content.length == 1) {
        isByteStreamBody(flattened.content(0))
      } else {
        false
      }
    private val onlyTheLastFieldIsStreaming =
      if (flattened.content.size > 1) {
        !flattened.content.init.exists(isByteStreamBody) && isByteStreamBody(flattened.content.last)
      } else {
        false
      }

    def decode(url: URL, status: Status, method: Method, headers: Headers, body: Body)(implicit
      trace: Trace,
    ): Task[Value] = ZIO.suspendSucceed {
      val inputsBuilder = flattened.makeInputsBuilder()

      decodePaths(url.path, inputsBuilder.path)
      decodeQuery(url.queryParams, inputsBuilder.query)
      decodeStatus(status, inputsBuilder.status)
      decodeMethod(method, inputsBuilder.method)
      decodeHeaders(headers, inputsBuilder.header)
      decodeBody(body, inputsBuilder.content).as(constructor(inputsBuilder))
    }

    final def encodeWith[Z](value: Value, outputTypes: Chunk[MediaTypeWithQFactor])(
      f: (URL, Option[Status], Option[Method], Headers, Body) => Z,
    ): Z = {
      val inputs = deconstructor(value)

      val path               = encodePath(inputs.path)
      val query              = encodeQuery(inputs.query)
      val status             = encodeStatus(inputs.status)
      val method             = encodeMethod(inputs.method)
      val headers            = encodeHeaders(inputs.header)
      def contentTypeHeaders = encodeContentType(inputs.content, outputTypes)
      val body               = encodeBody(inputs.content, outputTypes)

      val headers0 = if (headers.contains("content-type")) headers else headers ++ contentTypeHeaders
      f(URL(path, queryParams = query), status, method, headers0, body)
    }

    private def genericDecode[E, A, Codec](
      a: A, 
      codecs: Chunk[Codec],
      inputs: Array[Any],
      decode: (Codec, A) => A,
    ): Option[Unit] = {
      for( i <- 0 until inputs.length) {
        val codec = codecs(i)
        inputs(i) = decode(codec, a)
      }
    }

    private def decodePaths(path: Path, inputs: Array[Any]): Unit =
      genericDecode(
        path, 
        flattened.path, 
        inputs, 
        (codec, path) => {
          codec.decode(path) match {
            case Left(error) => throw HttpCodecError.MalformedPath(path, codec, error)
            case Right(value) => value
          }
        }
      )

    private def decodeQuery(queryParams: QueryParams, inputs: Array[Any]): Unit =
      genericDecode(
        queryParams,
        flattened.query,
        inputs, 
        (codec, queryParams) => {
          val params = queryParams.queryParamsOrElse(codec.name, Nil)

          if (params.isEmpty)
            throw HttpCodecError.MissingQueryParam(codec.name)
          else {
            val parsedParams = params.collect(codec.textCodec)
            inputs(i) = parsedParams
          }
        }
        )

    private def decodeStatus(status: Status, inputs: Array[Any]): Unit = 
      genericDecode(
        status,
        flattened.status,
        inputs, 
        (codec, status) =>
          codec match {
            case SimpleCodec.Specified(expected) if expected != status => throw HttpCodecError.MalformedStatus(expected, status)
            case _ => status
          }
        )

    private def decodeMethod(method: Method, inputs: Array[Any]): Unit = 
      genericDecode(
        method,
        flattened.method,
        inputs,
        (codec, method) =>
          codec match {
            case SimpleCodec.Specified(expected) if expected != method => throw HttpCodecError.MalformedMethod(expected, method)
            case _ => method
          }
      )


    lazy val codecsMapping: Map[String, ]
    

    private def decodeHeaders(headers: Headers, inputs: Array[Any]): Unit = 
      genericDecode(
        method,
        flattened.method,
        inputs,
        (codec, headers) =>
          headers.get(codec.name) match {
            case Some(value) =>
              codec.textCodec
                .decode(value)
                .getOrElse(throw HttpCodecError.MalformedHeader(codec.name, codec.textCodec))

            case None =>
              throw HttpCodecError.MissingHeader(codec.name)
          }
        )

    private def decodeBody(body: Body, inputs: Array[Any])(implicit
      trace: Trace,
    ): Task[Unit] = {
      val codecs = flattened.content

      if (inputs.length < 2) {
        //non multi-part
        codecs.headOption.map { codec =>
            codec
            .decodeFromBody(body)
            .mapBoth(
              { err => HttpCodecError.MalformedBody(err.getMessage(), Some(err)) },
              result => inputs(0) = result,
            )
        }.getOrElse(ZIO.unit)
      } else {
        //multi-part
        decodeForm(body.asMultipartFormStream, inputs) *> check
      }

    private def decodeForm(form: Form, inputs: Array[Any]): Unit = {

      val codecs = flattened.content
      for (field <- form.fields) {
        val i = codecsMapping.get(field.name).getOrElse(throw HttpCodecError.MalformedBody(s"Unexpected multipart/form-data field: ${field.name}"))
        val codec = codecs(i).erase
        val decoded = codec.decodeFromField(field)
        inputs(i) = decoded match {
          case Left(error) => throw error()
          // a, codec, error
          case Right(value) => value
        }

      }
    }

    private def check(): ZIO[Any, Throwable, Unit] =
      ZIO.attempt {
        inputs.map

        throw HttpCodecError.MalformedBody(
                  s"Missing multipart/form-data field (${Try(nameByIndex(idx))}",
                )
      }

    private def decodePaths(path: Path, inputs: Array[Any]): Unit = {
      assert(flattened.path.length == inputs.length)

      var i = 0

      while (i < inputs.length) {
        val pathCodec = flattened.path(i).erase

        val decoded = pathCodec.decode(path)

        inputs(i) = decoded match {
          case Left(error) =>
            throw HttpCodecError.MalformedPath(path, pathCodec, error)

          case Right(value) => value
        }

        i = i + 1
      }
    }

    private def decodeQuery(queryParams: QueryParams, inputs: Array[Any]): Unit = {
      var i       = 0
      val queries = flattened.query
      while (i < queries.length) {
        val query = queries(i).erase

        val params = queryParams.queryParamsOrElse(query.name, Nil)

        if (params.isEmpty)
          throw HttpCodecError.MissingQueryParam(query.name)
        else {
          val parsedParams = params.collect(query.textCodec)
          inputs(i) = parsedParams
        }

        i = i + 1
      }
    }

    private def decodeStatus(status: Status, inputs: Array[Any]): Unit = {
      var i = 0
      while (i < inputs.length) {
        inputs(i) = flattened.status(i) match {
          case _: SimpleCodec.Unspecified[_]   => status
          case SimpleCodec.Specified(expected) =>
            if (status != expected)
              throw HttpCodecError.MalformedStatus(expected, status)
            else ()
        }

        i = i + 1
      }
    }

    private def decodeMethod(method: Method, inputs: Array[Any]): Unit = {
      var i = 0
      while (i < inputs.length) {
        inputs(i) = flattened.method(i) match {
          case _: SimpleCodec.Unspecified[_]   => method
          case SimpleCodec.Specified(expected) =>
            if (method != expected) throw HttpCodecError.MalformedMethod(expected, method)
            else ()
        }

        i = i + 1
      }
    }

    private def decodeHeaders(headers: Headers, inputs: Array[Any]): Unit = {
      var i = 0
      while (i < flattened.header.length) {
        val header = flattened.header(i).erase

        headers.get(header.name) match {
          case Some(value) =>
            inputs(i) = header.textCodec
              .decode(value)
              .getOrElse(throw HttpCodecError.MalformedHeader(header.name, header.textCodec))

          case None =>
            throw HttpCodecError.MissingHeader(header.name)
        }

        i = i + 1
      }
    }

    private def decodeBody(body: Body, inputs: Array[Any])(implicit
      trace: Trace,
    ): Task[Unit] = {
      if (isByteStream) {
        ZIO.attempt(inputs(0) = body.asStream.orDie)
      } else if (flattened.content.isEmpty) {
        ZIO.unit
      } else if (flattened.content.size == 1) {
        val bodyCodec = flattened.content(0)
        bodyCodec
          .decodeFromBody(body)
          .mapBoth(
            { err => HttpCodecError.MalformedBody(err.getMessage(), Some(err)) },
            result => inputs(0) = result,
          )
      } else {
        body.asMultipartFormStream.flatMap { form =>
          if (onlyTheLastFieldIsStreaming)
            processStreamingForm(form, inputs)
          else
            collectAndProcessForm(form, inputs)
        }.zipRight {
          ZIO.attempt {
            var idx = 0
            while (idx < inputs.length) {
              if (inputs(idx) == null)
                throw HttpCodecError.MalformedBody(
                  s"Missing multipart/form-data field (${Try(nameByIndex(idx))}",
                )
              idx += 1
            }
          }
        }
      }
    }

    private def processStreamingForm(form: StreamingForm, inputs: Array[Any])(implicit
      trace: Trace,
    ): ZIO[Any, Throwable, Unit] =
      Promise.make[Throwable, Unit].flatMap { ready =>
        form.fields.mapZIO { field =>
          indexByName.get(field.name) match {
            case Some(idx) =>
              (flattened.content(idx) match {
                case BodyCodec.Multiple(codec, _) if codec.defaultMediaType.binary =>
                  field match {
                    case FormField.Binary(_, data, _, _, _)          =>
                      inputs(idx) = ZStream.fromChunk(data)
                    case FormField.StreamingBinary(_, _, _, _, data) =>
                      inputs(idx) = data
                    case FormField.Text(_, value, _, _)              =>
                      inputs(idx) = ZStream.fromChunk(Chunk.fromArray(value.getBytes(Charsets.Utf8)))
                    case FormField.Simple(_, value)                  =>
                      inputs(idx) = ZStream.fromChunk(Chunk.fromArray(value.getBytes(Charsets.Utf8)))
                  }
                  ZIO.unit
                case _                                                             =>
                  formFieldDecoders(idx)(field).map { result => inputs(idx) = result }
              })
                .zipRight(
                  ready
                    .succeed(())
                    .unless(
                      inputs.exists(_ == null),
                    ), // Marking as ready so the handler can start consuming the streaming field before this stream ends
                )
            case None      =>
              ready.fail(HttpCodecError.MalformedBody(s"Unexpected multipart/form-data field: ${field.name}"))
          }
        }.runDrain
          .intoPromise(ready)
          .forkDaemon
          .zipRight(
            ready.await,
          )
      }

    private def collectAndProcessForm(form: StreamingForm, inputs: Array[Any])(implicit
      trace: Trace,
    ): ZIO[Any, Throwable, Unit] =
      form.collectAll.flatMap { collectedForm =>
        ZIO.foreachDiscard(collectedForm.formData) { field =>
          indexByName.get(field.name) match {
            case Some(idx) =>
              flattened.content(idx) match {
                case BodyCodec.Multiple(codec, _) if codec.defaultMediaType.binary =>
                  field match {
                    case FormField.Binary(_, data, _, _, _)          =>
                      inputs(idx) = ZStream.fromChunk(data)
                    case FormField.StreamingBinary(_, _, _, _, data) =>
                      inputs(idx) = data
                    case FormField.Text(_, value, _, _)              =>
                      inputs(idx) = ZStream.fromChunk(Chunk.fromArray(value.getBytes(Charsets.Utf8)))
                    case FormField.Simple(_, value)                  =>
                      inputs(idx) = ZStream.fromChunk(Chunk.fromArray(value.getBytes(Charsets.Utf8)))
                  }
                  ZIO.unit
                case _                                                             =>
                  formFieldDecoders(idx)(field).map { result => inputs(idx) = result }
              }
            case None      =>
              ZIO.fail(HttpCodecError.MalformedBody(s"Unexpected multipart/form-data field: ${field.name}"))
          }
        }
      }

    private def genericEncode[T](codecs: Chunk[HttpCodec[_, A]], inputs: Array[Any], init: T, add: (T, T) => T): T = {
      var res = init
      for (i <- 0 until inputs.length) {
        val codec = codecs(i).erase
        val input = inputs(i)
        val encoded = codec.encode(input)
        res = add(res, encoded)
      }
      res
    }

    private def simpleEncode[A](codecs: Chunk[SimpleCodec[A]], inputs: Array[Any]): Option[A] = {
      codecs.headOption.map { codec =>
        codec match {
          case _: SimpleCodec.Unspecified[_] => Some(inputs(0).asInstanceOf[A])
          case SimpleCodec.Specified(elem) => Some(elem)
        }
      }

    private def encodePath(inputs: Array[Any]): Path =
      genericEncode(flattened.path, inputs, Path.empty, _ ++ _)

    private def encodeQuery(inputs: Array[Any]): QueryParams =
      genericEncode(flattened.query, inputs, QueryParams.empty, case (params, newParam) => params.addQueryParams(newParam.name, query.textCodec.encode()))

    private def encodeHeaders(inputs: Array[Any]): Headers =
      genericEncode(flattened.header, inputs, Headers.empty, case (headers, header) => headers ++ Headers(header.name, header.textCodec.encode))

    private def encodeStatus(inputs: Array[Any]): Option[Status] =
      simpleEncode(flattened.status, inputs)

    private def encodeMethod(inputs: Array[Any]): Option[Method] =
      simpleEncode(flattened.method, inputs)

    private def encodePath(inputs: Array[Any]): Path = {
      var path: Path = Path.empty

      var i = 0
      while (i < inputs.length) {
        val pathCodec = flattened.path(i).erase
        val input     = inputs(i)

        val encoded = pathCodec.encode(input) match {
          case Left(error)  =>
            throw HttpCodecError.MalformedPath(path, pathCodec, error)
          case Right(value) => value
        }
        path = path ++ encoded

        i = i + 1
      }

      path
    }

    private def encodeQuery(inputs: Array[Any]): QueryParams = {
      var queryParams = QueryParams.empty

      var i = 0
      while (i < inputs.length) {
        val query = flattened.query(i).erase
        val input = inputs(i)

        val inputCoerced = input.asInstanceOf[Chunk[Any]]

        if (inputCoerced.isEmpty)
          queryParams.addQueryParams(query.name, Chunk.empty[String])
        else
          inputCoerced.foreach { in =>
            val value = query.textCodec.encode(in)
            queryParams = queryParams.addQueryParam(query.name, value)
          }

        i = i + 1
      }

      queryParams
    }

    

    private def encodeHeaders(inputs: Array[Any]): Headers = {
      var headers = Headers.empty

      var i = 0
      while (i < inputs.length) {
        val header = flattened.header(i).erase
        val input  = inputs(i)

        val value = header.textCodec.encode(input)

        headers = headers ++ Headers(header.name, value)

        i = i + 1
      }

      headers
    }

    private def encodeBody(inputs: Array[Any], outputTypes: Chunk[MediaTypeWithQFactor]): Body =
      inputs.length match {
        case 0 =>
          Body.empty
        case 1 =>
          val bodyCodec = flattened.content(0)
          bodyCodec.erase.encodeToBody(inputs(0), outputTypes)
        case _ =>
          Body.fromMultipartForm(encodeMultipartFormData(inputs, outputTypes), formBoundary)
      }

    private def encodeMultipartFormData(inputs: Array[Any], outputTypes: Chunk[MediaTypeWithQFactor]): Form = {
      Form(
        flattened.content.zipWithIndex.map { case (bodyCodec, idx) =>
          val input = inputs(idx)
          val name  = nameByIndex(idx)
          bodyCodec match {
            case BodyCodec.Multiple(codec, _) if codec.defaultMediaType.binary =>
              FormField.streamingBinaryField(
                name,
                input.asInstanceOf[ZStream[Any, Nothing, Byte]],
                bodyCodec.mediaType(outputTypes).getOrElse(MediaType.application.`octet-stream`),
              )
            case _                                                             =>
              formFieldEncoders(idx)(name, input)
          }
        }: _*,
      )
    }

    private def encodeContentType(inputs: Array[Any], outputTypes: Chunk[MediaTypeWithQFactor]): Headers = {
      if (isByteStream) {
        val mediaType = flattened.content(0).mediaType(outputTypes).getOrElse(MediaType.application.`octet-stream`)
        Headers(Header.ContentType(mediaType))
      } else {
        if (inputs.length > 1) {
          Headers(Header.ContentType(MediaType.multipart.`form-data`))
        } else {
          if (flattened.content.length < 1) Headers.empty
          else {
            val mediaType = flattened
              .content(0)
              .mediaType(outputTypes)
              .getOrElse(throw HttpCodecError.CustomError("InvalidHttpContentCodec", "No codecs found."))
            Headers(Header.ContentType(mediaType))
          }
        }
      }
    }

    private def isByteStreamBody(codec: BodyCodec[_]): Boolean =
      codec match {
        case BodyCodec.Multiple(codec, _) if codec.defaultMediaType.binary => true
        case _                                                             => false
      }
  }

}
