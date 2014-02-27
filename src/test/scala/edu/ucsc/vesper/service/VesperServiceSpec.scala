package edu.ucsc.vesper.service

import org.specs2.mutable.Specification
import spray.testkit.Specs2RouteTest
import spray.http.{StatusCodes, BasicHttpCredentials, MediaTypes, HttpEntity}
import edu.ucsc.vesper.domain.Command

class VesperServiceSpec extends Specification with Specs2RouteTest with VesperService {

  def actorRefFactory = system

  val validCredential         = BasicHttpCredentials("legolas", "Q@nA53@T")
  val invalidCredential       = BasicHttpCredentials("bilbo",   "YeahBaby!")
  val unauthorizedCredential  = BasicHttpCredentials("smaug",   "O@nC35@P")


  "VesperService" should {
    "return a query for GET requests to the root path" in {
      Get("/?q=blue") ~> addCredentials(validCredential) ~> vesperRoute ~> check {
        responseAs[String] must contain("The query is")
      }
    }

    "leave GET requests to other paths unhandled" in {
      Get("/aha") ~> vesperRoute ~> check {
        handled must beFalse
      }
    }

    "return a command for PUT requests to the root path" in {
      Put("/", Command("add", List("class A{}"))) ~>
        addCredentials(validCredential) ~>
        sealRoute(vesperRoute) ~> check {
          responseAs[Command] == Command("add", List("class A{}"))
        }
    }

    "return a command for POST requests to the root path" in {
      Post("/", Command("show", List("origin"))) ~>
        addCredentials(validCredential) ~>
        sealRoute(vesperRoute) ~> check {
          responseAs[Command] == Command("show", List("origin"))
        }
    }

    "return a command in JSON form for POST requests to the root path" in {
      Post("/?c=", HttpEntity(MediaTypes.`application/json`, """{ "name": "show", "params" : ["origin"] }""" )) ~>
        addCredentials(validCredential) ~>
        sealRoute(vesperRoute) ~> check {
          responseAs[Command] === Command("show", List("origin"))
        }
    }

    "come back with a 401 unauthorized request message" in {
      Get("/?q=blue") ~> addCredentials(invalidCredential) ~> sealRoute(vesperRoute) ~> check {
        status === StatusCodes.Unauthorized
        responseAs[String] === "The supplied authentication is invalid"
      }
    }

    "come back with a 405 unauthenticated request message" in {
      Put("/", Command("add", List("class A{}"))) ~>
        addCredentials(unauthorizedCredential) ~>
        sealRoute(vesperRoute) ~> check {
          status === StatusCodes.Forbidden
          responseAs[String] === "The supplied authentication is not authorized to access this resource"
        }

    }
  }
}