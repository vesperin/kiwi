package edu.ucsc.vesper.http.api

import org.specs2.mutable.Specification
import spray.testkit.Specs2RouteTest
import spray.http.{StatusCodes, HttpEntity, MediaTypes}
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
        responseAs[ChangeSummary].draft.get.after  ===  Code(None,"Preconditions.java","Resource Injector",None,"class Preconditions {\n\tvoid inject(Object object) {\n\t}\n}",None)
      }
    }


    "return a rename member request for POST requests to the root path" in {
      Post("/api/try?auth_token=legolas", Command(rename = Some(Rename("member", where =  List(6, 15), to = "Preconditions", source = Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}"))))) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[ChangeSummary].draft.get.before ===  Code(None,"Bootstrap.java","Resource Injector",None,"class Bootstrap {void inject(Object object){}}",None)
        responseAs[ChangeSummary].draft.get.after  ===  Code(None,"Preconditions.java","Resource Injector",None,"class Preconditions {\n\tvoid inject(Object object) {\n\t}\n}",None)
      }
    }


    "return a rename class request in JSON form for POST requests to the root path" in {
      Post("/api/try?c", HttpEntity(MediaTypes.`application/json`, """{"rename": { "what": "class", "where":[6, 15], "to": "Preconditions", "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"class Bootstrap {void inject(Object object){}}"} }}""" )) ~>
        addHeader(RawHeader("x-auth-token", "legolas")) ~>
        sealRoute(vesperRoutes) ~> check {

        responseAs[ChangeSummary].draft.get.before ===  Code(None,"Bootstrap.java","Resource Injector",None,"class Bootstrap {void inject(Object object){}}",None)
        responseAs[ChangeSummary].draft.get.after  ===  Code(None,"Preconditions.java","Resource Injector",None,"class Preconditions {\n\tvoid inject(Object object) {\n\t}\n}",None)

      }
    }

    "return a remove method request in JSON form for POST requests to the root path" in {
      Post("/api/try?auth_token=legolas", HttpEntity(MediaTypes.`application/json`, """{"remove": { "what": "method", "where":[17, 45], "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"class Bootstrap {void inject(Object object){}}"} }}""" )) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[ChangeSummary].draft.get.before ===  Code(None,"Bootstrap.java","Resource Injector",None,"class Bootstrap {void inject(Object object){}}",None)
        responseAs[ChangeSummary].draft.get.after  ===  Code(None,"Bootstrap.java","Resource Injector",None,"class Bootstrap {}",None)
      }
    }

    "return a remove method request for POST requests to the root path" in {
      Post("/api/try?auth_token=legolas", Command(remove = Some(Remove("method", where =  List(17, 45), source = Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}"))))) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[ChangeSummary].draft.get.before ===  Code(None,"Bootstrap.java","Resource Injector",None,"class Bootstrap {void inject(Object object){}}",None)
        responseAs[ChangeSummary].draft.get.after  ===  Code(None,"Bootstrap.java","Resource Injector",None,"class Bootstrap {}",None)
      }
    }

    "return an optimize imports request in JSON form for POST requests to the root path" in {
      Post("/api/try?auth_token=legolas", HttpEntity(MediaTypes.`application/json`, """{"optimize": { "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"import java.util.List;\t\n\tclass Bootstrap {void inject(Object object){}}"} }}""" )) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[ChangeSummary].draft.get.before ===  Code(None,"Bootstrap.java","Resource Injector",None,"import java.util.List;\t\n\tclass Bootstrap {void inject(Object object){}}",None)
        responseAs[ChangeSummary].draft.get.after  ===  Code(None,"Bootstrap.java","Resource Injector",None,"class Bootstrap {void inject(Object object){}}",None)
      }
    }

    "return an optimize imports request for POST requests to the root path" in {
      Post("/api/try?auth_token=legolas", Command(optimize = Some(Optimize(source = Code(name = "Bootstrap.java", description = "Resource Injector", content = "import java.util.List;\t\n\tclass Bootstrap {void inject(Object object){}}"))))) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[ChangeSummary].draft.get.before ===  Code(None,"Bootstrap.java","Resource Injector",None,"import java.util.List;\t\n\tclass Bootstrap {void inject(Object object){}}",None)
        responseAs[ChangeSummary].draft.get.after  ===  Code(None,"Bootstrap.java","Resource Injector",None,"class Bootstrap {void inject(Object object){}}",None)
      }
    }

    "return a formatting request for POST requests to the root path" in {
      Post("/api/try?auth_token=legolas", Command(format = Some(Format(Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}"))))) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[ChangeSummary].draft.get.before === Code(None,"Bootstrap.java","Resource Injector",None,"class Bootstrap {void inject(Object object){}}",None)
        responseAs[ChangeSummary].draft.get.after  === Code(None,"Bootstrap.java","Resource Injector",None,"class Bootstrap {\n\tvoid inject(Object object) {\n\t}\n}",None)
      }
    }

    "return a formatting request in JSON form for POST requests to the root path" in {
      Post("/api/try?auth_token=legolas", HttpEntity(MediaTypes.`application/json`, """{"format": { "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"class Bootstrap {void inject(Object object){}}"} }}""" )) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[ChangeSummary].draft.get.before === Code(None,"Bootstrap.java","Resource Injector",None,"class Bootstrap {void inject(Object object){}}",None)
        responseAs[ChangeSummary].draft.get.after  === Code(None,"Bootstrap.java","Resource Injector",None,"class Bootstrap {\n\tvoid inject(Object object) {\n\t}\n}",None)
      }
    }

    "return a deduplicate request for POST requests to the root path" in {
      Post("/api/try?auth_token=legolas", Command(deduplicate = Some(Deduplicate(Code(name = "Name.java", description = "Name class", content = "class Name {\n\t/** {@link Name#boom(String)} **/\tvoid boom(){ System.out.println(1); }\n\tvoid baam(){ System.out.println(1); }\n\tvoid beem(){ System.out.println(1); }\n\tvoid buum(){ baam(); }\n}"))))) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[ChangeSummary].draft.get.before === Code(name = "Name.java", description = "Name class", content = "class Name {\n\t/** {@link Name#boom(String)} **/\tvoid boom(){ System.out.println(1); }\n\tvoid baam(){ System.out.println(1); }\n\tvoid beem(){ System.out.println(1); }\n\tvoid buum(){ baam(); }\n}")
        responseAs[ChangeSummary].draft.get.after  === Code(name = "Name.java", description = "Name class", content = "class Name {\n\t/** {@link Name#boom(String)} **/\tvoid boom(){ System.out.println(1); }\n\tvoid buum(){ boom(); }\n}")
      }
    }

    "return a deduplicate request in JSON form for POST requests to the root path" in {
      Post("/api/try?auth_token=legolas", HttpEntity(MediaTypes.`application/json`, """{"deduplicate": { "source": {"name": "Name.java", "description":"Name class", "content":"class Name {\n\t/** {@link Name#boom(String)} **/\tvoid boom(){ System.out.println(1); }\n\tvoid baam(){ System.out.println(1); }\n\tvoid beem(){ System.out.println(1); }\n\tvoid buum(){ baam(); }\n}"} }}""" )) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[ChangeSummary].draft.get.before === Code(name = "Name.java", description = "Name class", content = "class Name {\n\t/** {@link Name#boom(String)} **/\tvoid boom(){ System.out.println(1); }\n\tvoid baam(){ System.out.println(1); }\n\tvoid beem(){ System.out.println(1); }\n\tvoid buum(){ baam(); }\n}")
        responseAs[ChangeSummary].draft.get.after  === Code(name = "Name.java", description = "Name class", content = "class Name {\n\t/** {@link Name#boom(String)} **/\tvoid boom(){ System.out.println(1); }\n\tvoid buum(){ boom(); }\n}")
      }
    }


    "return a cleanup request for POST requests to the root path" in {
      Post("/api/try?auth_token=legolas", Command(cleanup = Some(Cleanup(Code(name = "Name.java", description = "Name class", content = "class Name {\n\t/** {@link Name#boom(String)} **/\tvoid boom(){ System.out.println(1); }\n\tvoid baam(){ System.out.println(1); }\n\tvoid beem(){ System.out.println(1); }\n\tvoid buum(){ baam(); }\n}"))))) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[ChangeSummary].draft.get.before === Code(name = "Name.java", description = "Name class", content = "class Name {\n\t/** {@link Name#boom(String)} **/\tvoid boom(){ System.out.println(1); }\n\tvoid baam(){ System.out.println(1); }\n\tvoid beem(){ System.out.println(1); }\n\tvoid buum(){ baam(); }\n}")
        responseAs[ChangeSummary].draft.get.after  === Code(name = "Name.java", description = "Name class", content = "class Name {\n" + "\t/** {@link Name#boom(String)} **/\n" + "\tvoid boom() {\n" + "\t\tSystem.out.println(1);\n" + "\t}\n" + "\n" + "\tvoid buum() {\n" + "\t\tboom();\n" + "\t}\n" + "}")
      }
    }

    "return a cleanup request in JSON form for POST requests to the root path" in {
      Post("/api/try?auth_token=legolas", HttpEntity(MediaTypes.`application/json`, """{"cleanup": { "source": {"name": "Name.java", "description":"Name class", "content":"class Name {\n\t/** {@link Name#boom(String)} **/\tvoid boom(){ System.out.println(1); }\n\tvoid baam(){ System.out.println(1); }\n\tvoid beem(){ System.out.println(1); }\n\tvoid buum(){ baam(); }\n}"} }}""" )) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[ChangeSummary].draft.get.before === Code(name = "Name.java", description = "Name class", content = "class Name {\n\t/** {@link Name#boom(String)} **/\tvoid boom(){ System.out.println(1); }\n\tvoid baam(){ System.out.println(1); }\n\tvoid beem(){ System.out.println(1); }\n\tvoid buum(){ baam(); }\n}")
        responseAs[ChangeSummary].draft.get.after  === Code(name = "Name.java", description = "Name class", content = "class Name {\n" + "\t/** {@link Name#boom(String)} **/\n" + "\tvoid boom() {\n" + "\t\tSystem.out.println(1);\n" + "\t}\n" + "\n" + "\tvoid buum() {\n" + "\t\tboom();\n" + "\t}\n" + "}")
      }
    }

    "come back with a custom unauthorized request message" in {
      Get("/api/search?q=curators") ~> addHeader(RawHeader("x-auth-token", "gollum")) ~> sealRoute(vesperRoutes) ~> check {
        status === StatusCodes.OK
        responseAs[Answer].items(0) === "You are not authorized to see these resources."
      }
    }

    "come back with a 401 unauthorized request message" in {
      Put("/api/try", Command(deduplicate = Some(Deduplicate(Code(name = "Name.java", description = "Name class", content = "class Name {\n\t/** {@link Name#boom(String)} **/\tvoid boom(){ System.out.println(1); }\n\tvoid baam(){ System.out.println(1); }\n\tvoid beem(){ System.out.println(1); }\n\tvoid buum(){ baam(); }\n}"))))) ~>
        addHeader(RawHeader("x-auth-token", "mal")) ~>
        sealRoute(vesperRoutes) ~> check {
        status === StatusCodes.Unauthorized
      }

    }
  }

}
