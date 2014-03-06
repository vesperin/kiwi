package edu.ucsc.vesper.http.api

import org.specs2.mutable.Specification
import spray.testkit.Specs2RouteTest
import spray.http.{MediaTypes, HttpEntity}
import edu.ucsc.vesper.http.domain.LoungeObjects._
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.TimeUnit._
import spray.http.HttpHeaders.RawHeader

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
class VesperinSpec extends Specification with Specs2RouteTest with Vesperin {
  def actorRefFactory = system

  // reason of this addition? see https://groups.google.com/forum/#!msg/spray-user/o8DtI6VUMbA/n9tguTb_1noJ
  implicit val routeTestTimeout = RouteTestTimeout(FiniteDuration(5, SECONDS))

  "Vesperin" should {
    "return a greeting for GET requests to the 'all' path" in {
      Get("/api/hi") ~> vesperRoutes ~> check {
        responseAs[String] must contain("Hello")
      }
    }

    "leave GET requests to other paths unhandled" in {
      Get("/aha") ~> vesperRoutes ~> check {
        handled must beFalse
      }
    }

    "return a query for GET requests containing authorization HTTP Header to the root path" in {
      Get("/api/search?q=curators") ~> addHeader(RawHeader("x-auth-token", "legolas")) ~> vesperRoutes ~> check {
        responseAs[Answer] === Answer(List("aragorn", "galadriel", "frodo", "legolas", "gandalf"))
      }
    }

    "return a query, containing an authentication token, for GET requests to the root path" in {
      Get("/api/search?auth_token=legolas&q=reviewers") ~> vesperRoutes ~> check {
        responseAs[Answer] === Answer(List("sauron", "thranduil", "gollum", "smaug", "melkor"))
      }
    }

    "return an answer for Get request to the root path stating the user is not authorized to see the requested resources" in {
      Get("/api/search?q=everybody&auth_token=gollum") ~> vesperRoutes ~> check {
        responseAs[Answer] === Answer(List("You are not authorized to see these resources."))
      }
    }

    "return an inspect request in JSON form for POST requests to the root path" in {
      Post("/api/try?c", HttpEntity(MediaTypes.`application/json`, """{"inspect": { "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"class Bootstrap {void inject(Object object){}}"} }}""" )) ~>
        addHeader(RawHeader("x-auth-token", "legolas")) ~>
        sealRoute(vesperRoutes) ~> check {

        val answers = List(
          ChangeSummary(None,Some(List(Warning("Unused method","Vesper has detected one or more unused methods!",Some(List(17, 45))), Warning("Unused parameter","Vesper has detected unused parameters in one or more methods",Some(List(29, 42))))),None),
          ChangeSummary(None,Some(List(Warning("Unused parameter","Vesper has detected unused parameters in one or more methods",Some(List(29, 42))), Warning("Unused method","Vesper has detected one or more unused methods!",Some(List(17, 45))))),None)
        )

        answers.contains(responseAs[ChangeSummary])
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
