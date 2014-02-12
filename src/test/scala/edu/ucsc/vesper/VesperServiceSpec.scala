package edu.ucsc.vesper

import org.specs2.mutable.Specification
import spray.testkit.Specs2RouteTest

class VesperServiceSpec extends Specification with Specs2RouteTest with VesperService {

  def actorRefFactory = system

  "VesperService" should {
    "return a command for GET requests to the root path" in {
      Get("/?q=blue") ~> vesperRoutes ~> check {
        responseAs[String] must contain("The query is")
      }
    }

    "leave GET requests to other paths unhandled" in {
      Get("/aha") ~> vesperRoutes ~> check {
        handled must beFalse
      }
    }

    "return a MethodNotAllowed error for PUT requests to the root path" in {
      Put("/", "blue") ~> sealRoute(vesperRoutes) ~> check {
        responseAs[String] must contain("The command is")
      }
    }
  }
}