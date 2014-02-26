package edu.ucsc.vesper.service

import org.specs2.mutable.Specification
import spray.testkit.Specs2RouteTest
import spray.http.{StatusCodes, BasicHttpCredentials, MediaTypes, HttpEntity}
import edu.ucsc.vesper.domain.Command

class VesperServiceSpec extends Specification with Specs2RouteTest with VesperService {

  def actorRefFactory = system

  val drafted   = BasicHttpCredentials("drafted",   "A@nC35@R")
  val undrafted = BasicHttpCredentials("undrafted", "YeahBaby!")


  "VesperService" should {
    "return a query for GET requests to the root path" in {
      Get("/?q=blue") ~> addCredentials(drafted) ~> vesperRoute ~> check {
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
        addCredentials(drafted) ~>
        sealRoute(vesperRoute) ~> check {
          responseAs[Command] == Command("add", List("class A{}"))
        }
    }

    "return a command for POST requests to the root path" in {
      Post("/", Command("show", List("origin"))) ~>
        addCredentials(drafted) ~>
        sealRoute(vesperRoute) ~> check {
          responseAs[Command] == Command("show", List("origin"))
        }
    }

    "return a command in JSON form for POST requests to the root path" in {
      Post("/?c=", HttpEntity(MediaTypes.`application/json`, """{ "name": "show", "params" : ["origin"] }""" )) ~>
        addCredentials(drafted) ~>
        sealRoute(vesperRoute) ~> check {
          responseAs[Command] === Command("show", List("origin"))
        }
    }

    "come back with a 401 unauthorized request message" in {
      Get("/?q=blue") ~> addCredentials(undrafted) ~> sealRoute(vesperRoute) ~> check {
        status === StatusCodes.Unauthorized
        responseAs[String] === "The supplied authentication is invalid"
      }
    }
  }
}