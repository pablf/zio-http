package zio.http

import zio._
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
      }.provide(ZLayer.succeed(Server.Config.default)),
      test("error in body") {
        val res = routes.deploy.body.mapZIO(_.asString).run(path = Path.root / "error")
        assertZIO(res)(isNotEmptyString)
      }.provide(ZLayer.succeed(Server.Config.default.errorInBody(true))),
    )
      .provide(
        DynamicServer.live,
        Server.customized,
        ZLayer.succeed(NettyConfig.defaultWithFastShutdown),
        Client.default,
      ) @@ sequential @@ withLiveClock
}
