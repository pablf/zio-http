package zio.http.security

import zio.test._

import zio.schema._

import zio.http._
import zio.http.codec._
import zio.http.endpoint._

object TimingAttacksSpec extends ZIOSpecDefault {
    // maybe vulnerabilities from timeouts
    // vulnerabilities from chaining of middlewares

    val nOfTries = 1000
    val statisticNeeded = ???
    def statistics[A](a: () => A): (Double, Double) = {
        var sample = List.empty
        for (i <- 0 to nOfTries) {
            val timeBefore = ???
            a
            val timeAfter = ???
            sample = (timeAfter - timeBefore) :: sample
        }
        val mean = sample.fold(_ + _)/nOfTries
        val variance = sample.map(s => (s-mean)^2).fold(_ + _)/nOfTries
        (mean, variance)
    }

    def vulnerabiltyFound[A](a: () => A, b: () => A): Boolean = {
        // check p value
        val (meanA, varianceA) = statistics(a)
        val (meanB, varianceB) = statistics(b)
        val testStatistic = (meanA-meanB)*(nOfTries*(nOfTries-1))^(1/2)/(varianceA+varianceB)
        testStatistic > statisticNeeded
    }

    val spec = suite("ExceptionSpec")(
        // extends Error
        test("ClientError"){},
        test("ServerError"){},
        // extends Exception
        test("FromDecodingError"){},
        test("QueryParamsError"){},
        test("EndpointNotFound"){},
        test("HttpCodecError"){},
        // extends Throwable
        test("RouteFailure"){}

    )
}

