package zio.http

import zio._
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

import zio.http.internal.{DynamicServer, HttpRunnableSpec}
import zio.http.netty.NettyConfig

object ErrorInBodySpec extends HttpRunnableSpec {

  private val routes = Routes(Method.GET / "test" -> Handler.ok.map(_ => throw new Throwable("ERROR")))

  private val app = serve(routes)

  def notInBodySpec =
    suite("ErrorNotInBodySpec") {
      val tests = test("error not in body") {
        assertZIO(for {
          port   <- DynamicServer.port
          client <- ZIO.service[Client]
          url = URL.decode("http://localhost:%d/%s".format(port, Path.root / "test")).toOption.get
          body    <- client(Request(url = url)).map(_.body)
          content <- body.asString
        } yield content)(isEmptyString)
      }
      ZIO.scoped(app.as(List(tests)))
    }.provideSome[DynamicServer & Server & Client](Scope.default)
      .provideShared(
        ZLayer.succeed(Server.Config.default),
        DynamicServer.live,
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
