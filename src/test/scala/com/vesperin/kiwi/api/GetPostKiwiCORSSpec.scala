package com.vesperin.kiwi.api

import org.specs2.matcher.Matchers
import org.specs2.mutable.Specification
import spray.routing.HttpService
import spray.testkit.Specs2RouteTest

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
class GetPostKiwiCORSSpec extends Specification
with Specs2RouteTest with HttpService with CORS with Matchers {

  def actorRefFactory = system

  val testRoute = path("test") {
    cors {
      get { complete((200, "CORS it works!")) } ~
      post { complete((200, "CORS I'll update that!")) }
    }
  }

  "Kiwi" should {
    "Return CORS it works for a Get Request " in {
      Get("/test") ~> testRoute ~> check {

        status.intValue shouldEqual  200
        responseAs[String] mustEqual "CORS it works!"
      }
    }

    "Return CORS I'll update that! for a Post Request " in {
      Post("/test") ~> testRoute ~> check {
        status.intValue shouldEqual  200
        responseAs[String] mustEqual "CORS I'll update that!"
      }
    }

    "Respond to OPTIONS requests properly" in {
      Options("/test") ~> testRoute ~> check {
        println(header("Access-Control-Allow-Methods").get)

        val allowMethods = header("Access-Control-Allow-Methods").get.value.split(", ")
        Array("OPTIONS", "POST", "GET") foreach { allowMethods contains  _ }
        Array("PUT", "DELETE") foreach { allowMethods contains  _ !== true }

        status.intValue mustEqual 200

        header("Access-Control-Allow-Headers").isDefined mustEqual true
        header("Access-Control-Max-Age").isDefined mustEqual true

      }
    }

  }

}
