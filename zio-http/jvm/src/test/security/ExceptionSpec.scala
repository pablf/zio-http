package zio.http.security

import zio.test._

import zio.schema._

import zio.http._
import zio.http.codec._
import zio.http.endpoint._

object ExceptionSpec extends ZIOSpecDefault {

    val routesError = Routes(Method.GET / "error" -> handler.ok.map(_ => throw new Throwable("BOOM!")))
    val routesFail = Routes(Method.GET / "fail" -> handler.fail(new Throwable("BOOM!")))

    val queryRoutes =
    Routes(
      Method.GET / "search" -> Handler.fromFunctionHandler { (req: Request) =>
        val response: ZIO[Any, QueryParamsError, Response] =
          ZIO.fromEither(req.queryParamTo[Int]("age"))
             .map(value => Response.text(s"The value of age query param is: $value"))

        Handler.fromZIO(response).catchAll {
          case QueryParamsError.Missing(name)                  =>
            Handler.badRequest(s"The $name query param is missing")
          case QueryParamsError.Malformed(name, codec, values) =>
            Handler.badRequest(s"The value of $name query param is malformed")
        }
      },
    )

    val spec = suite("ExceptionSpec")(
        test("leaks stacktrace"){
            for {
                _ <- Server.install(routesError)
                client <- ZIO.service[Client]
                response <- client(req)
            } yield assertTrue(!response.contains("Throwable"))
        },
        test("leaks stacktrace"){
            for {
                _ <- Server.install(routesFail)
                client <- ZIO.service[Client]
                response <- client(req)
            } yield assertTrue(!response.contains("Throwable"))
        },
        test("QueryParamsError"){
            test("leaks stacktrace"){
            for {
                _ <- Server.install(queryRoutes)
                client <- ZIO.service[Client]
                response <- client(req)
            } yield assertTrue(!response.contains("Throwable"))
        },
        },
        test("HttpCodecError"){???},
        // doesn't seem possible to leak: EndpointNotFound, ClientError, FormDecodingError (because it is a decoding done by the server)
        // for sure not: RouteFailure
    )
}
