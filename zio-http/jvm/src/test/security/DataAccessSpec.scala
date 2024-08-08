package zio.http.security

import zio.test._

import zio.schema._

import zio.http._
import zio.http.codec._
import zio.http.endpoint._

object DataAccessSpec extends ZIOSpecDefault {
    //check access to configuration, system properties,...
    //checking middlewares and json zio schema???
    val spec = suite("ExceptionSpec")(
    )
}
