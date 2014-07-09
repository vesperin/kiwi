package edu.ucsc.vesper.http.api

import java.util.Date

import edu.ucsc.vesper.http.domain.Models._
import org.specs2.mutable.Specification
import spray.http.HttpHeaders.RawHeader
import spray.http.StatusCodes
import spray.testkit.Specs2RouteTest

import scala.concurrent.duration._

/**
 * This class covers any DB tests for vesper; e.g., storing code snippet, finding code snippet, rendering
 * a nice HTML pages for a code snippet, etc.
 *
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 *
 */
class VesperinDBSupportSpec extends Specification with Specs2RouteTest with Vesperin {
  def actorRefFactory = system

  // reason of this addition? see https://groups.google.com/forum/#!msg/spray-user/o8DtI6VUMbA/n9tguTb_1noJ
  implicit val routeTestTimeout = RouteTestTimeout(FiniteDuration(5, SECONDS))

  def cleanupDatabase = {
    interpreter.storage.clear()
  }

  "Vesperin-with-mongo" should {
    "return Code snippet was saved! on a put request and perform a database insert " in {
      Put("/kiwi/eval", Command(persist = Some(Persist(
        Code(
          name = "GCD.java",
          description     = "Implements GCD algorithm /by @codedetour.",
          content         = "class GCD {\t\t\n\t\tstatic int computeGcd(int a, int b){\n\t\t return BigInteger.valueOf(a).gcd(BigInteger.valueOf(b)).intValue(); \n\t\t}\n}",
          elapsedtime     = Some("00:05:00"),
          tags            = List("Slow"),
          datastructures  = List("BigInteger"),
          algorithms      = List("GCD"),
          refactorings    = List("Full clean up"),
          confidence      = 2,
          url             = Some("http://www.programmingtask.com"),
          birthday        = Some(new Date().getTime),
          comments        = List(Comment(from = "1;1;0", to = "2;10;234", text = "BigInteger's gcd seems very expensive"))))))) ~>
        addHeader(RawHeader("x-auth-token", "legolas")) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[Result] mustEqual Result(info = Some(Info(List("GCD.java was saved, then tweeted by @codetour"))))
      }
    }

    "return a Code Result Set containing matching Tags for GET request to the root path" in {
      Get("/kiwi/find?q=tags:CrackingTheCodeInterview") ~> addHeader(RawHeader("x-auth-token", "gollum")) ~> sealRoute(vesperRoutes) ~> check {
        responseAs[Result].sources.get.nonEmpty mustEqual true
      }
    }

    "return a Code Result Set containing matching Algs for GET request to the root path" in {
      Get("/kiwi/find?q=algs:GCD") ~> addHeader(RawHeader("x-auth-token", "gollum")) ~> sealRoute(vesperRoutes) ~> check {
        responseAs[Result].sources.get.nonEmpty mustEqual true
      }
    }

    "return a single Code Result Set containing matching id for GET request to the root path" in {
      Get("/kiwi/find?q=id:89956303-635e-4449-b6a4-ab3cc603143c") ~> addHeader(RawHeader("x-auth-token", "gollum")) ~> sealRoute(vesperRoutes) ~> check {

        status === StatusCodes.OK
        val res: Result = responseAs[Result]
        res.sources.get.nonEmpty mustEqual true

      }
    }

    "render HTML of a single Code Result Set containing matching id for GET request to the root path" in {
      Get("/kiwi/render?q=id:89956303-635e-4449-b6a4-ab3cc603143c") ~> addHeader(RawHeader("x-auth-token", "gollum")) ~> sealRoute(vesperRoutes) ~> check {

        status === StatusCodes.OK
      }
    }
  }

}
