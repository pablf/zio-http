package zio.http.security

import zio.test._

import zio.schema._

import zio._
import zio.http._
import zio.http.codec._
import zio.http.endpoint._
import zio.http.netty.NettyConfig
import zio.metrics.Metrics

object OutOfMemorySpec extends ZIOHttpSpec {

    val infiniteURL = Gen.string.map(s => url"$s")
    val infiniteURLs = Gen.chunkOf(Gen.stringN(1)(Gen.char))
    //val headers = Gen.fromIterable(???)
    //val infiniteHeader = Gen.string.zip(headers).map( (content, header) => Header)
    //val infiniteTotalHeaders = ???
    val infiniteBodyString = Gen.string.map(s => Body.fromString(s))
    val infiniteBodyFile = Gen.string.map(s => Body.fromString(s))
    val infiniteBodyStream = Gen.string.map(s => Body.fromString(s))
    val infiniteBody = infiniteBodyString ++ infiniteBodyFile ++ infiniteBodyStream
    val infiniteTotalBodies = Gen.chunkOf(Gen.stringN(1)(Gen.char))
    // tests metrics:
    /*
        Middleware.metrics()
        memoryUsage
    */

    def memoryUsage: ZIO[Any, Nothing, Double] = {
        import java.lang.Runtime._
        ZIO
        .succeed(getRuntime.totalMemory() - getRuntime.freeMemory())
        .map(_ / (1024.0 * 1024.0)) @@ Metrics.gauge("memory_usage")
    }

    val routes = Routes(Method.GET / "test" -> Handler.ok)
    val longResponseRoute = Routes(Method.GET / "test" -> Handler.ok)
    val heavyRoute = Routes(Method.GET / "test" -> Handler.ok)

    val MAX_URL_SIZE = 1000
    val MAX_HEADER_SIZE = 1000
    val MAX_CONTENT_SIZE = 1000

    val spec = suite("OutOfMemorySpec")(
        suite("configurable limits work")(
            test("urls") {
                for {
                    _ <- Server.install(routes)
                    result <- check(infiniteURLs) { path =>
                        for {
                            client <- ZIO.service[Client]
                            response <- client(url = path).map(_.status)
                            expected = if (path.size > MAX_URL_SIZE) Status.Ok else Status.Ok
                        } yield assertZIO(response)(equalTo(expected))
                    }
                } yield result
            },
        ).provide(
            Server.customized,
            ZLayer.succeed(
                Server.Config.default
                    .maxHeaderSize(MAX_HEADER_SIZE)
                    .maxInitialLineLength(MAX_URL_SIZE)
                    .disableRequestStreaming(MAX_CONTENT_SIZE)
                ),
            ZLayer.succeed(NettyConfig.defaultWithFastShutdown),
            Client.default,
        ),
        suite("testing default limits")(
            /*
                body length -> disableRequestStreaming/enableRequestStreaming
                total header length -> maxHeaderSize
                url -> maxInitialLine
            */
            test("urls") {
                for {
                    _ <- Server.install(routes)
                    result <- check(infiniteURLs) { path =>
                        for {
                            client <- ZIO.service[Client]
                            response <- client(url = path).map(_.status)
                        } yield assertZIO(response)(equalTo(Status.Ok))
                    }
                } yield result
            },
            /*test("header") {
                _ <- Server.install(routes)
                    result <- check(infiniteHeader) { headers =>
                        for {
                            client <- ZIO.service[Client]
                            response <- client(headers = headers).map(_.status)
                        } yield assertZIO(res)(equalTo(Status.Ok))
                }
            },
            test("headers") {
                _ <- Server.install(routes)
                    result <- check(infiniteTotalHeaders) { headers =>
                    val res = routes.deploy.status.run(headers = headers)
                    assertZIO(res)(equalTo(Status.Ok))
                }
            },
            test("body") {
                _ <- Server.install(routes)
                    result <- check(infiniteBody) { body =>
                    val res = routes.deploy.status.run(body = body)
                    assertZIO(res)(equalTo(Status.Ok))
                }
            },
            test("multi-form bodies") {
                _ <- Server.install(routes)
                    result <- check(infiniteTotalBodies) { bodies =>
                    val res = routes.deploy.status.run(body = bodies)
                    assertZIO(res)(equalTo(Status.Ok))
                }
            },*/
        ).provide(
            Server.customized,
            ZLayer.succeed(NettyConfig.defaultWithFastShutdown),
            Client.default,
        ),
        suite("metrics")(
            test("urls") {
                for {
                    _ <- Server.install(routes)
                    result <- check(infiniteURLs) { path =>
                        for {
                            client <- ZIO.service[Client]
                            response <- client(url = path).map(_.status)
                        } yield assertZIO(response)(equalTo(Status.Ok))
                    }
                    usage <- metrics
                } yield assertTrue(usage < 1000)
            },
        ).provide(
            Server.customized,
            ZLayer.succeed(NettyConfig.defaultWithFastShutdown),
            Client.default,
            prometheus.prometheusLayer,
            prometheus.publisherLayer,
            ZLayer.succeed(MetricsConfig(5.seconds))
        ),
    ) @@ TestAspect.sample(100000) @@ TestAspect.sequential

}