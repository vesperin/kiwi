package com.vesperin.kiwi.api

import com.vesperin.kiwi.domain.Result
import org.specs2.mutable.Specification
import spray.http.HttpHeaders.RawHeader
import spray.http.StatusCodes
import spray.testkit.Specs2RouteTest

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
class GetKiwiApiSpec extends Specification with Specs2RouteTest with Kiwi  {
  def actorRefFactory = system


  "Kiwi" should {
    "Return a greeting for GET requests to the 'all' paths" in {
      Get("/kiwi/help") ~> routes ~> check {
        responseAs[String] must contain("Code Transformation Functionality")
      }
    }


    "Return a custom unauthorized request message" in {
      Get("/kiwi/find?q=roles:curators") ~> addHeader(RawHeader("x-auth-token", "gollum")) ~> sealRoute(routes) ~> check {
        status === StatusCodes.OK
        responseAs[Result].failure.get.message === "You are not authorized to see these resources."
      }
    }
  }
}
