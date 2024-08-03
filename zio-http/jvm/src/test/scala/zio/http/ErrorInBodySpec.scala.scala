package zio.http

import zio._
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

import zio.http.internal.HttpRunnableSpec
import zio.http.netty.NettyConfig

object ErrorInBodySpec extends ZIOHttpSpec {

  def notInBodySpec =
    suite("ErrorNotInBodySpec")(
      test("error not in body by default") {
        val routes = Routes(Method.GET / "test" -> Handler.ok.map(_ => throw new Throwable("Error")))
        assertZIO(for {
          port   <- Server.install(routes)
          client <- ZIO.service[Client]
          url = URL.decode("http://localhost:%d/%s".format(port, Path.root / "test")).toOption.get
          body    <- client(Request(url = url)).map(_.body)
          content <- body.asString
        } yield content)(isEmptyString)
      },
      test("include error in body") {
        val routes = Routes(Method.GET / "test" -> Handler.ok.map(_ => throw new Throwable("Error")))
        assertZIO(for {
          port   <- Server.install(routes.includeErrorDetails)
          client <- ZIO.service[Client]
          url = URL.decode("http://localhost:%d/%s".format(port, Path.root / "test")).toOption.get
          body    <- client(Request(url = url)).map(_.body)
          content <- body.asString
        } yield content)(not(isEmptyString))
      },
      test("exclude error in body") {
        val routes = Routes(Method.GET / "test" -> Handler.ok.map(_ => throw new Throwable("Error")))
        assertZIO(for {
          port   <- Server.install(routes.includeErrorDetails.excludeErrorDetails)
          client <- ZIO.service[Client]
          url = URL.decode("http://localhost:%d/%s".format(port, Path.root / "test")).toOption.get
          body    <- client(Request(url = url)).map(_.body)
          content <- body.asString
        } yield content)(isEmptyString)
      },
    ).provideSome[Server & Client](Scope.default)
      .provideShared(
        ZLayer.succeed(Server.Config.default),
        Server.customized,
        ZLayer.succeed(NettyConfig.defaultWithFastShutdown),
        Client.default,
      ) @@ sequential @@ withLiveClock

  /*def inBodySpec =
    suite("ErrorInBodySpec") {
      val tests = test("error in body") {
        val res = routes.deploy.body.mapZIO(_.asString).run(path = Path.root / "test")
        assertZIO(res)(not(isEmptyString))
      }
      ZIO.scoped(app.as(List(tests)))
    }.provideSome[DynamicServer & Server & Client](Scope.default)
      .provideShared(
        ZLayer.succeed(Server.Config.default.errorInBody(true)),
        DynamicServer.live,
        Server.customized,
        ZLayer.succeed(NettyConfig.defaultWithFastShutdown),
        Client.default,
      ) @@ sequential @@ withLiveClock*/

  override def spec =
    suite("ErrorInBodySpec")(
      notInBodySpec,
      // inBodySpec,
    ) @@ sequential @@ withLiveClock

}
