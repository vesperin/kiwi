package edu.ucsc.vesper.http.api

import org.specs2.mutable.Specification
import spray.testkit.Specs2RouteTest
import spray.http.HttpHeaders.RawHeader
import spray.http.{MediaTypes, HttpEntity}
import edu.ucsc.vesper.http.domain.LoungeObjects._

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
class VesperinSpec extends Specification with Specs2RouteTest with Vesperin {
  def actorRefFactory = system

  "Vesperin" should {
    "return a greeting for GET requests to the 'all' path" in {
      Get("/api/all") ~> vesperRoutes ~> check {
        responseAs[String] must contain("Morning")
      }
    }

    "leave GET requests to other paths unhandled" in {
      Get("/aha") ~> vesperRoutes ~> check {
        handled must beFalse
      }
    }

    "return a query for GET requests containing authorization HTTP Header to the root path" in {
      Get("/api/try?q=curators") ~> addHeader(RawHeader("x-auth-token", "legolas")) ~> vesperRoutes ~> check {
        responseAs[Answer] === Answer(List("aragorn", "galadriel", "frodo", "legolas", "gandalf"))
      }
    }

    "return a query, containing an authentication token, for GET requests to the root path" in {
      Get("/api/try?auth_token=legolas&q=reviewers") ~> vesperRoutes ~> check {
        responseAs[Answer] === Answer(List("sauron", "thranduil", "gollum", "smaug", "melkor"))
      }
    }


//    "return an inspect request in JSON form for POST requests to the root path" in {
//      Post("/api/try?c", HttpEntity(MediaTypes.`application/json`, """{"inspect": { "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"class Bootstrap {void inject(Object object}{}}"} }}""" )) ~>
//        addHeader(RawHeader("x-auth-token", "legolas")) ~>
//        sealRoute(vesperRoutes) ~> check {
//        responseAs[Command] === Command(
//          inspect = Some(
//            Inspect(
//              Code(
//                name        = "Bootstrap.java",
//                description = "Resource Injector",
//                content     = "class Bootstrap {void inject(Object object}{}}"
//              )
//            )
//          ),
//          None
//        )
//      }
//    }
//
//
//    "return an inspect request for POST requests to the root path" in {
//      Post("/api/try", Command(inspect = Some(Inspect(Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object}{}}"))))) ~>
//        addHeader(RawHeader("x-auth-token", "legolas")) ~>
//        sealRoute(vesperRoutes) ~> check {
//        responseAs[Command] === Command(
//          inspect = Some(
//            Inspect(
//              Code(
//                name        = "Bootstrap.java",
//                description = "Resource Injector",
//                content     = "class Bootstrap {void inject(Object object}{}}"
//              )
//            )
//          ),
//          None
//        )
//      }
//    }
//
//    "return an inspect request with authorization token for POST requests to the root path" in {
//      Post("/api/try?auth_token=legolas", Command(inspect = Some(Inspect(Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object}{}}"))))) ~>
//        sealRoute(vesperRoutes) ~> check {
//        responseAs[Command] === Command(
//          inspect = Some(
//            Inspect(
//              Code(
//                name        = "Bootstrap.java",
//                description = "Resource Injector",
//                content     = "class Bootstrap {void inject(Object object}{}}"
//              )
//            )
//          ),
//          None
//        )
//      }
//    }
//
//    "return an inspect request with authorization token for PUT requests to the root path" in {
//      Put("/api/try?auth_token=legolas", Command(inspect = Some(Inspect(Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object}{}}"))))) ~>
//        sealRoute(vesperRoutes) ~> check {
//        responseAs[Command] === Command(
//          inspect = Some(
//            Inspect(
//              Code(
//                name        = "Bootstrap.java",
//                description = "Resource Injector",
//                content     = "class Bootstrap {void inject(Object object}{}}"
//              )
//            )
//          ),
//          None
//        )
//      }
//    }
//
  }

}
