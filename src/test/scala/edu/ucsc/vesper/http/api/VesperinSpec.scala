package edu.ucsc.vesper.http.api

import org.specs2.mutable.Specification
import spray.testkit.Specs2RouteTest
import spray.http.{MediaTypes, HttpEntity}
import edu.ucsc.vesper.http.domain.LoungeObjects._
import scala.concurrent.duration._
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
          ChangeSummary(warnings = Some(List(Warning("object", "Unused parameter", Some(List(29, 42))), Warning("inject", "Unused method", Some(List(17, 45)))))),
          ChangeSummary(warnings = Some(List(Warning("inject", "Unused method", Some(List(17, 45))), Warning("object", "Unused parameter", Some(List(29, 42))))))
        )

        answers.contains(responseAs[ChangeSummary])
      }
    }

    "return an inspect request for POST requests to the root path" in {
      Post("/api/try?auth_token=legolas", Command(inspect = Some(Inspect(Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}"))))) ~>
        sealRoute(vesperRoutes) ~> check {

        val answers = List(
          ChangeSummary(warnings = Some(List(Warning("object", "Unused parameter", Some(List(29, 42))), Warning("inject", "Unused method", Some(List(17, 45)))))),
          ChangeSummary(warnings = Some(List(Warning("inject", "Unused method", Some(List(17, 45))), Warning("object", "Unused parameter", Some(List(29, 42))))))
        )

        answers.contains(responseAs[ChangeSummary])
      }
    }


    "return a rename class request for POST requests to the root path" in {
      Post("/api/try?auth_token=legolas", Command(rename = Some(Rename("class", where =  List(6, 15), to = "Preconditions", source = Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}"))))) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[ChangeSummary].draft.get.before ===  Code(None,"Bootstrap.java","Resource Injector",None,"class Bootstrap {void inject(Object object){}}",None)
        responseAs[ChangeSummary].draft.get.after  ===  Code(None,"Preconditions.java","Resource Injector",None,"class Preconditions { void inject(Object object) { } }",None)
      }
    }


    "return a rename class request in JSON form for POST requests to the root path" in {
      Post("/api/try?c", HttpEntity(MediaTypes.`application/json`, """{"rename": { "what": "class", "where":[6, 15], "to": "Preconditions", "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"class Bootstrap {void inject(Object object){}}"} }}""" )) ~>
        addHeader(RawHeader("x-auth-token", "legolas")) ~>
        sealRoute(vesperRoutes) ~> check {

        responseAs[ChangeSummary].draft.get.before ===  Code(None,"Bootstrap.java","Resource Injector",None,"class Bootstrap {void inject(Object object){}}",None)
        responseAs[ChangeSummary].draft.get.after  ===  Code(None,"Preconditions.java","Resource Injector",None,"class Preconditions { void inject(Object object) { } }",None)

      }
    }

    "return a remove method request in JSON form for POST requests to the root path" in {
      Post("/api/try?auth_token=legolas", HttpEntity(MediaTypes.`application/json`, """{"remove": { "what": "method", "where":[17, 45], "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"class Bootstrap {void inject(Object object){}}"} }}""" )) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[ChangeSummary].draft.get.before ===  Code(None,"Bootstrap.java","Resource Injector",None,"class Bootstrap {void inject(Object object){}}",None)
        responseAs[ChangeSummary].draft.get.after  ===  Code(None,"Bootstrap.java","Resource Injector",None,"class Bootstrap { }",None)
      }
    }

    "return a remove method request for POST requests to the root path" in {
      Post("/api/try?auth_token=legolas", Command(remove = Some(Remove("method", where =  List(17, 45), source = Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}"))))) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[ChangeSummary].draft.get.before ===  Code(None,"Bootstrap.java","Resource Injector",None,"class Bootstrap {void inject(Object object){}}",None)
        responseAs[ChangeSummary].draft.get.after  ===  Code(None,"Bootstrap.java","Resource Injector",None,"class Bootstrap { }",None)
      }
    }

    "return an optimize imports request in JSON form for POST requests to the root path" in {
      Post("/api/try?auth_token=legolas", HttpEntity(MediaTypes.`application/json`, """{"optimize": { "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"import java.util.List; \n class Bootstrap {void inject(Object object){}}"} }}""" )) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[ChangeSummary].draft.get.before ===  Code(None,"Bootstrap.java","Resource Injector",None,"import java.util.List;   class Bootstrap {void inject(Object object){}}",None)
        responseAs[ChangeSummary].draft.get.after  ===  Code(None,"Bootstrap.java","Resource Injector",None,"class Bootstrap { void inject(Object object) { } }",None)
      }
    }

    "return an optimize imports request for POST requests to the root path" in {
      Post("/api/try?auth_token=legolas", Command(optimize = Some(Optimize(source = Code(name = "Bootstrap.java", description = "Resource Injector", content = "import java.util.List; \n class Bootstrap {void inject(Object object){}}"))))) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[ChangeSummary].draft.get.before ===  Code(None,"Bootstrap.java","Resource Injector",None,"import java.util.List;   class Bootstrap {void inject(Object object){}}",None)
        responseAs[ChangeSummary].draft.get.after  ===  Code(None,"Bootstrap.java","Resource Injector",None,"class Bootstrap { void inject(Object object) { } }",None)
      }
    }

  }

}
