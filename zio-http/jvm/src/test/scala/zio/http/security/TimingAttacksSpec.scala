package zio.http.security

import zio.Config.Secret
import zio._
import zio.test.Assertion._
import zio.test._

import zio.schema._

import zio.http._
import zio.http.codec._
import zio.http.endpoint._

object TimingAttacksSpec extends ZIOSpecDefault {

  // uses Box test from "Opportunities and Limits of Remote Timing Attacks", Scott A. Crosby, Dan S. Wallach, Rudolf H. Riedi
  val i = 0.01
  val j = 0.3

  val nOfTries = 1000

  def runZ[A](a: ZIO[Any, Throwable, A]) =
    Unsafe.unsafe { implicit unsafe =>
      zio.Runtime.default.unsafe
        .run(
          a,
        )
        .getOrThrowFiberFailure()
    }

  // not use when measuring `Handlers` performance
  def boxTest[A](slow: ZIO[Any, Throwable, A], fast: ZIO[Any, Throwable, A]): ZIO[Any, Throwable, Boolean] =
    for {
      statisticsA <- statistics(() => slow)
      statisticsB <- statistics(() => fast)
    } yield !(statisticsB._2 < statisticsA._1)

  def statistics[A](a: () => ZIO[Any, Throwable, A]): ZIO[Any, Throwable, (Long, Long)] =
    ZIO.loop(0)(_ < nOfTries, _ + 1)(i => measure(a())).map { sampleUnsorted =>
      val sample = sampleUnsorted.sorted
      val tail   = sample.drop((nOfTries * i).round.toInt)
      val low    = tail.head
      val high   = tail.drop((nOfTries * (j - i)).round.toInt).head
      (low, high)
    }

  def measure[A](f: ZIO[Any, Throwable, A]): ZIO[Any, Throwable, Long] =
    for {
      before <- Clock.nanoTime
      res    <- f
      after  <- Clock.nanoTime
    } yield after - before

  // after a few iterations, boxTest doesn't detect differences accurately when dealing with `Handlers`
  def boxTest2[A](
    h: () => Handler[Any, Nothing, Request, Response],
    slow: Request,
    fast: Request,
  ): ZIO[Any, Throwable, Boolean] =
    ZIO.attempt {
      val statisticsA = statistics2(h, slow)
      val statisticsB = statistics2(h, fast)
      val diff        = statisticsA._1 - statisticsB._2
      println(s"$statisticsA, $statisticsB")
      !(diff > statisticsB._2 / 20)
    }

  def statistics2[A](h: () => Handler[Any, Nothing, Request, Response], slow: Request): (Long, Long) = {
    var sampleUnsorted = List.empty[Long]
    for (i <- 0 until nOfTries) {
      val b = java.lang.System.nanoTime
      runZ { h().runZIO(slow) }
      val a = java.lang.System.nanoTime
      sampleUnsorted = (a - b) :: sampleUnsorted
    }
    val sample = sampleUnsorted.sorted
    val tail = sample.drop((nOfTries * i).round.toInt)
    val low  = tail.head
    val high = tail.drop((nOfTries * (j - i)).round.toInt).head
    (low, high)
  }

  val passwd = "some-secret" * 1000 ++ "-"

  val basicAuthM     = HandlerAspect.basicAuth { c => Secret(passwd) equals c.upassword }
  val basicAuthM2    = HandlerAspect.basicAuth("user", passwd)
  val basicAuthZIOM  = HandlerAspect.basicAuthZIO { c => ZIO.succeed(Secret(passwd) equals c.upassword) }
  val bearerAuthM    = HandlerAspect.bearerAuth { token => Secret(passwd) equals token }
  val bearerAuthZIOM = HandlerAspect.bearerAuthZIO { token => ZIO.succeed(Secret(passwd) equals token) }

  def basicAuthApp()     = (Handler.ok @@ basicAuthM).merge
  def basicAuthApp2()    = (Handler.ok @@ basicAuthM2).merge
  def basicAuthZIOApp()  = (Handler.ok @@ basicAuthZIOM).merge
  def bearerAuthApp()    = (Handler.ok @@ basicAuthM).merge
  def bearerAuthZIOApp() = (Handler.ok @@ basicAuthZIOM).merge

  private val sameLengthHeaderBasic = Header.Authorization.Basic("user", "some-secrez" * 1000 ++ "-")
  private val goodHeaderBasic       = Header.Authorization.Basic("user", "some-secret" * 1000 ++ "-")
  private val almostGoodHeaderBasic = Header.Authorization.Basic("user", "some-secret" * 1000 ++ "a")
  private val badHeaderBasic        = Header.Authorization.Basic("user", "a")

  private val sameLengthHeaderBearer = Header.Authorization.Bearer("some-secrez" * 1000 ++ "-")
  private val goodHeaderBearer       = Header.Authorization.Bearer("some-secret" * 1000 ++ "-")
  private val almostGoodHeaderBearer = Header.Authorization.Bearer("some-secret" * 1000 ++ "a")
  private val badHeaderBearer        = Header.Authorization.Bearer("a")

  def suiteFor(name: String)(
    app: () => Handler[Any, Nothing, Request, Response],
    goodRequest: Request,
    almostGoodRequest: Request,
    badRequest: Request,
    sameLengthRequest: Request,
  ) =
    suite(name)(
      test("doesn't leak length") {
        assertZIO(boxTest2(app, sameLengthRequest, badRequest))(equalTo(true))
      },
      test("doesn't leak secrets") {
        assertZIO(boxTest2(app, goodRequest, badRequest))(equalTo(true))
      },
      test("doesn't leak secrets - same length") {
        assertZIO(boxTest2(app, goodRequest, sameLengthRequest))(equalTo(true))
      },
      test("doesn't leak parts of secret") {
        assertZIO(boxTest2(app, goodRequest, almostGoodRequest))(equalTo(true))
      },
    )

  def runApp(app: () => Handler[Any, Nothing, Request, Response], req: Request): Any =
    runZ { app().runZIO(req) }

  val sameLengthReqBasic = Request.get(URL.empty).addHeader(sameLengthHeaderBasic)
  val goodReqBasic       = Request.get(URL.empty).addHeader(goodHeaderBasic)
  val almostGoodReqBasic = Request.get(URL.empty).addHeader(almostGoodHeaderBasic)
  val badReqBasic        = Request.get(URL.empty).addHeader(badHeaderBasic)

  val sameLengthReqBearer = Request.get(URL.empty).addHeader(sameLengthHeaderBearer)
  val goodReqBearer       = Request.get(URL.empty).addHeader(goodHeaderBearer)
  val almostGoodReqBearer = Request.get(URL.empty).addHeader(almostGoodHeaderBearer)
  val badReqBearer        = Request.get(URL.empty).addHeader(badHeaderBearer)

  val spec = suite("TimingAttackSpec")(
    suiteFor("basicAuth")(basicAuthApp _, goodReqBasic, almostGoodReqBasic, badReqBasic, sameLengthReqBasic),
    suiteFor("basicAuth2")(basicAuthApp2 _, goodReqBasic, almostGoodReqBasic, badReqBasic, sameLengthReqBasic),
    suiteFor("basicAuthZIO")(basicAuthZIOApp _, goodReqBasic, almostGoodReqBasic, badReqBasic, sameLengthReqBasic),
    suiteFor("bearerAuth")(bearerAuthApp _, goodReqBearer, almostGoodReqBearer, badReqBearer, sameLengthReqBearer),
    suiteFor("bearerAuthZIO")(
      bearerAuthZIOApp _,
      goodReqBearer,
      almostGoodReqBearer,
      badReqBearer,
      sameLengthReqBearer,
    ),
    /*test("basicAuth doesn't leak that user is wrong, good password") {

      val basicAuthM = HandlerAspect.basicAuth("user", passwd)
      val req1       = Request.get(url"").addHeader(Header.Authorization.Basic("user", passwd))
      val req2       = Request.get(url"").addHeader(Header.Authorization.Basic("badUser", passwd))

      def app() = (Handler.ok @@ basicAuthM).merge
      assertZIO(boxTest2(app _, req1, req2))(equalTo(false))
    } @@ TestAspect.failing,*/
    test("basicAuth doesn't leak that user is wrong, bad password") {

      val basicAuthM = HandlerAspect.basicAuth("user", passwd)
      val req1       = Request.get(url"").addHeader(Header.Authorization.Basic("user", "passwd"))
      val req2       = Request.get(url"").addHeader(Header.Authorization.Basic("badUser", "passwd"))

      def app() = (Handler.ok @@ basicAuthM).merge
      assertZIO(boxTest2(app _, req1, req2))(equalTo(true))
    },
    test("Secret vulnerability") {
      val secret          = zio.Config.Secret("some-secret" * 1000)
      val differentLength = zio.Config.Secret("some-secre" * 1000)
      val sameLength      = zio.Config.Secret("some-secrez" * 1000)
      assertZIO(boxTest(ZIO.attempt { secret equals sameLength }, ZIO.attempt { secret equals differentLength }))(
        equalTo(true),
      )
    } @@ TestAspect.failing,
    test("Secret vulnerability inverted") {
      val secret          = zio.Config.Secret("some-secret" * 1000)
      val differentLength = zio.Config.Secret("some-secre" * 1000)
      val sameLength      = zio.Config.Secret("some-secrez" * 1000)
      assertZIO(boxTest(ZIO.attempt { sameLength equals secret }, ZIO.attempt { differentLength equals secret }))(
        equalTo(true),
      )
    } @@ TestAspect.failing,
    test("Secret non vulnerability") {
      val secret     = zio.Config.Secret("some-secret" * 1000)
      val sameLength = zio.Config.Secret("some-secrez" * 1000)
      assertZIO(boxTest(ZIO.attempt { secret equals secret }, ZIO.attempt { secret equals sameLength }))(equalTo(true))
    },
  ) @@ TestAspect.sequential @@ TestAspect.withLiveClock @@ TestAspect.flaky
}
