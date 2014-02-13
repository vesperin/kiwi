package edu.ucsc.vesper

import org.specs2.mutable.Specification
import spray.testkit.Specs2RouteTest
import spray.http.{MediaTypes, HttpEntity}

class VesperServiceSpec extends Specification with Specs2RouteTest with VesperService {

  def actorRefFactory = system

  "VesperService" should {
    "return a query for GET requests to the root path" in {
      Get("/?q=blue") ~> vesperRoutes ~> check {
        responseAs[String] must contain("The query is")
      }
    }

    "return a find and replace query for GET requests to the root path" in {
      // ?q=[test~=hello] {{r}}.{{l}}
      Get("/?q=" + """%5Btest%7E%3Dhello%5D+%7B%7Br%7D%7D.%7B%7Bl%7D%7D""") ~> vesperRoutes ~> check {
        responseAs[String] must contain("The query is")
      }
    }

    "leave GET requests to other paths unhandled" in {
      Get("/aha") ~> vesperRoutes ~> check {
        handled must beFalse
      }
    }

    "return a command for PUT requests to the root path" in {
      Put("/", Command("add", "class A{}")) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[Command] == Command("add", "class A{}")
      }
    }

    "return a command for POST requests to the root path" in {
      Post("/", Command("show", "origin")) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[Command] == Command("show", "origin")
      }
    }

    "return a command in JSON form for POST request to the root path" in {
      Post("/?c=", HttpEntity(MediaTypes.`application/json`, """{ "name": "show", "params" : "origin" }""" )) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[Command] === Command("show", "origin")
      }
    }
  }
}