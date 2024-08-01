package zio.http

import zio._
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

import zio.http.internal.{DynamicServer, HttpRunnableSpec}
import zio.http.netty.NettyConfig

object ErrorInBodySpec extends HttpRunnableSpec {

  val routes = Routes(Method.GET / "test" -> Handler.ok)

  override def spec =
    suite("ErrorInBodySpec")(
      test("error not in body") {
        val res = routes.deploy.body.mapZIO(_.asString).run(path = Path.root / "error")
        assertZIO(res)(isEmptyString)
      }.provideSome(
        ZLayer.succeed(Server.Config.default),
        Scope.default,
        DynamicServer.live,
        Server.customized,
        ZLayer.succeed(NettyConfig.defaultWithFastShutdown),
        Client.default,
      ),
      test("error in body") {
        val res = routes.deploy.body.mapZIO(_.asString).run(path = Path.root / "error")
        assertZIO(res)(not(isEmptyString))
      }.provideSome(
        ZLayer.succeed(Server.Config.default),
        Scope.default,
        DynamicServer.live,
        Server.customized,
        ZLayer.succeed(NettyConfig.defaultWithFastShutdown),
        Client.default,
      ),
    ) @@ sequential @@ withLiveClock
}
