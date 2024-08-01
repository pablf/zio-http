package zio.http

import zio._
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

import zio.http.internal.{DynamicServer, HttpRunnableSpec}
import zio.http.netty.NettyConfig

object ErrorInBodySpec extends HttpRunnableSpec {

  private val app = serve

  private val routes = Routes(Method.GET / "test" -> Handler.ok)

  def notInBodySpec =
    suite("ErrorNotInBodySpec") {
      val tests = test("error not in body") {
        val res = routes.deploy.body.mapZIO(_.asString).run(path = Path.root / "error")
        assertZIO(res)(isEmptyString)
      }
      suite("app without request streaming") { ZIO.scoped(app.as(List(tests))) }
    }.provideSome[DynamicServer & Server & Client](Scope.default)
      .provideShared(
        ZLayer.succeed(Server.Config.default),
        DynamicServer.live,
        Server.customized,
        ZLayer.succeed(NettyConfig.defaultWithFastShutdown),
        Client.default,
      ) @@ sequential @@ withLiveClock

  def inBodySpec =
    suite("ErrorInBodySpec") {
      val tests = test("error in body") {
        val res = routes.deploy.body.mapZIO(_.asString).run(path = Path.root / "error")
        assertZIO(res)(not(isEmptyString))
      }
      suite("app without request streaming") { ZIO.scoped(app.as(List(tests))) }
    }.provideSome[DynamicServer & Server & Client](Scope.default)
      .provideShared(
        ZLayer.succeed(Server.Config.default.errorInBody(true)),
        DynamicServer.live,
        Server.customized,
        ZLayer.succeed(NettyConfig.defaultWithFastShutdown),
        Client.default,
      ) @@ sequential @@ withLiveClock

  override def spec =
    suite("ErrorInBodySpec")(
      notInBodySpec,
      inBodySpec,
    ) @@ sequential @@ withLiveClock

}
