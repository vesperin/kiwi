package edu.ucsc.vesper

import org.specs2.mutable.Specification
import spray.testkit.Specs2RouteTest
import spray.http._
import StatusCodes._

class VesperServiceSpec extends Specification with Specs2RouteTest with VesperService {
	def actorRefFactory = system
	
	"VesperService" should {
	    "return a command for GET requests to the root path" in {
	      Get("/?q=blue") ~> vesperRoutes ~> check {
	        responseAs[String] must contain("The command is")
	      }
	    }
		
	    "leave GET requests to other paths unhandled" in {
	      Get("/aha") ~> vesperRoutes ~> check {
	        handled must beFalse
	      }
	    }
		
	    "return a MethodNotAllowed error for PUT requests to the root path" in {
	      Put() ~> sealRoute(vesperRoutes) ~> check {
	        status === MethodNotAllowed
	        responseAs[String] === "HTTP method not allowed, supported methods: GET"
	      }
	    }
	}
}