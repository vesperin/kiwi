package com.vesperin.kiwi.api

import com.vesperin.kiwi.domain._
import org.specs2.mutable.Specification
import spray.http.HttpHeaders.RawHeader
import spray.http.StatusCodes
import spray.testkit.Specs2RouteTest

import scala.concurrent.duration._

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
class GetKiwiApiWithDatabaseSpec extends Specification with Specs2RouteTest with Kiwi {
  def actorRefFactory = system

  // reason of this addition? see https://groups.google.com/forum/#!msg/spray-user/o8DtI6VUMbA/n9tguTb_1noJ
  implicit val routeTestTimeout = RouteTestTimeout(FiniteDuration(5, SECONDS))

  "Kiwi" should {

    "Return a Code Result Set containing matching Tags for GET request to the root path" in {
      Get("/kiwi/find?q=tags:CrackingTheCodeInterview") ~> addHeader(RawHeader("x-auth-token", "gollum")) ~> sealRoute(routes) ~> check {
        responseAs[Result].sources.get.nonEmpty mustEqual true
      }
    }

    "Return a Code Result Set containing matching Algs for GET request to the root path" in {
      Get("/kiwi/find?q=algs:GCD") ~> addHeader(RawHeader("x-auth-token", "gollum")) ~> sealRoute(routes) ~> check {
        responseAs[Result].sources.get.nonEmpty mustEqual true
      }
    }

    "Return a single Code Result Set containing matching id for GET request to the root path" in {
      Get("/kiwi/find?q=id:89956303-635e-4449-b6a4-ab3cc603143c") ~> addHeader(RawHeader("x-auth-token", "gollum")) ~> sealRoute(routes) ~> check {

        status === StatusCodes.OK
        val res: Result = responseAs[Result]
        res.sources.get.nonEmpty mustEqual true

      }
    }

    "Return HTML (with survey) of a single Code Result Set containing matching id for GET request to the root path" in {
      Get("/kiwi/render?q=id:89956303-635e-4449-b6a4-ab3cc603143c") ~> addHeader(RawHeader("x-auth-token", "gollum")) ~> sealRoute(routes) ~> check {

        status === StatusCodes.OK
      }
    }

    "Return HTML (without survey) of a single Code Result Set containing matching id for GET request to the root path" in {
      Get("/kiwi/render?q=id:89956303-635e-4449-b6a4-ab3cc603143c&s=off") ~> addHeader(RawHeader("x-auth-token", "gollum")) ~> sealRoute(routes) ~> check {
        status === StatusCodes.OK
      }
    }
  }
}
