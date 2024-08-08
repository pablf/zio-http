package zio.http.security

import zio.test._

import zio.schema._

import zio.http._
import zio.http.codec._
import zio.http.endpoint._
import zio.http.template.Dom

object UserDataSpec extends ZIOSpecDefault {
  // check vulnerabilities in name of uploaded files (https://cheatsheetseries.owasp.org/cheatsheets/File_Upload_Cheat_Sheet.html#filename-sanitization)
  //upload and download limits???
  //sql injections??? (https://cheatsheetseries.owasp.org/cheatsheets/Injection_Prevention_Cheat_Sheet.html#how-to-test-for-the-issue_2)

    val msg = "<script>alert('XSS');</script>"
    val mediaTypeGen = Gen.fromIterable(List(MediaType.text.`html`))

    val tuples = Gen.fromIterable(
      List(
        (MediaType.text.`html`, "<script>alert('XSS');</script>", "%3Cscript%3Ealert(%27XSS%27);%3C/script%3E"),
        (MediaType.text.`html`, "&", "&amp;"),
        (MediaType.text.`html`, "<", "&lt;"),
        (MediaType.text.`html`, ">", "&gt;"),
        (MediaType.text.`html`, "\"", "&quot;"),
        (MediaType.text.`html`, "'", "&#x27;"),
        (MediaType.text.`html`, "<script>alert('XSS');</script>", "%3Cscript%3Ealert(%27XSS%27);%3C/script%3E"),
        (MediaType.text.`html`, "<script>alert('XSS');</script>", "%3Cscript%3Ealert(%27XSS%27);%3C/script%3E"),
        (MediaType.text.`html`, "<script>alert('XSS');</script>", "%3Cscript%3Ealert(%27XSS%27);%3C/script%3E"),
        (MediaType.text.`javascript`, "<script>alert('XSS');</script>", "%3Cscript%3Ealert(%27XSS%27);%3C/script%3E"),
        (MediaType.text.`yaml`, "<script>alert('XSS');</script>", "%3Cscript%3Ealert(%27XSS%27);%3C/script%3E"),
        (MediaType.text.`xml`, "<script>alert('XSS');</script>", "%3Cscript%3Ealert(%27XSS%27);%3C/script%3E"),
        (MediaType.text.`css`, "<script>alert('XSS');</script>", "%3Cscript%3Ealert(%27XSS%27);%3C/script%3E"),
      )
    )

    val functions = Gen.fromIterable(List(
      (s: String) => Dom.element("html", Dom.raw(s)),
      (s: String) => Dom.element("div", Dom.raw(s)),
    ))

    /*
      - DOM-based XSS vulnerabilities are client-side and thus difficult to prevent. Any client-side vulnerable code could be sent by the server.
      Some clients implement escaping before sending requests and ZIO-HTTP could make some tools to make this possible. ??? Search headers that make this possible

      - HTML server-side XSS is covered unless using `DOM.raw`. I would opt to indicate that using `DOM.raw` might be unsafe.

    */

    val spec = suite("UserDataSpec")(
        /*test("Header injection"){
            val badHeader = ""
            val expected = ""
            val endpoint     = Endpoint(Method.GET / "test")
          .query(QueryCodec.queryInt("age"))
          .out[Unit]
        val route        = endpoint.implementHandler(headerHandler(identity))
        val request1      =
          Request(method = Method.GET, url = url"/test?age=1&age=2").addHeader(Header.Accept(MediaType.text.`html`))
        val expectedBody =
          html(
            body(
              h1("Codec Error"),
              p("There was an error en-/decoding the request/response"),
              p("SchemaTransformationFailure", idAttr                                      := "name"),
              p("Expected single value for query parameter age, but got 2 instead", idAttr := "message"),
            ),
          )
        for {
          response <- route.toRoutes.runZIO(request)
          body     <- response.body.asString
        } yield assertTrue(body == expectedBody.encode.toString)
        },*/
        test("Path injection"){
          check(tuples.zip(functions)) { case (mediaType, msg, expectedResponse, f) =>
            val request = Request(url = url"/$msg").addHeader(Header.Accept(mediaType))
            val expectedResponse = ""
            val endpoint     = Endpoint(Method.GET)
              .in[String]
              .out[String]
            val route        = endpoint.implementHandler(Handler.fromFunction(b =>f(b.asString)))
            for {
              response <- route.toRoutes.runZIO(request)
              body     <- response.body.asString
            } yield assertTrue(body == expectedResponse)
          }  
        },
        test("Body injection"){
          check(tuples.zip(functions)) { case (mediaType, msg, expectedResponse, f) =>
            val body = Body.fromString(msg)
            val request = Request(body = body).addHeader(Header.Accept(mediaType))
            val endpoint     = Endpoint(Method.GET)
              .in[String]
              .out[String]
            val route        = endpoint.implementHandler(Handler.body.fromFunction(b => f(b.asString)))
            for {
              response <- route.toRoutes.runZIO(request)
              body     <- response.body.asString
            } yield assertTrue(body == expectedResponse)
          }  
        },
        test("Error injection"){
          check(tuples.zip(functions)) { case (mediaType, msg, expectedResponse, f) =>
            val body = Body.fromString(msg)
            val request = Request(body = body).addHeader(Header.Accept(mediaType))
            val endpoint     = Endpoint(Method.GET)
              .in[String]
              .out[String]
            val route        = endpoint.implementHandler(Handler.error(Status.Ok, msg))
            for {
              response <- route.toRoutes.runZIO(request)
              body     <- response.body.asString
            } yield assertTrue(body == expectedResponse)
          }  
        },
    )


    //check authentication parse header error propagation
}
