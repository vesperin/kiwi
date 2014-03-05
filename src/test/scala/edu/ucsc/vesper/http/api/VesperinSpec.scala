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

  "Vesper" should {
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
      Get("/api/try?q=blue") ~> addHeader(RawHeader("x-auth-token", "legolas")) ~> vesperRoutes ~> check {
        responseAs[String] must contain("Morning")
      }
    }

    "return a query, containing an authentication token, for GET requests to the root path" in {
      Get("/api/try?auth_token=legolas&q=blue") ~> vesperRoutes ~> check {
        responseAs[String] must contain("Morning")
      }
    }


    "return an inspect request in JSON form for POST requests to the root path" in {
      Post("/api/try?c", HttpEntity(MediaTypes.`application/json`, """{"inspect": { "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"class Bootstrap {void inject(Object object}{}}"} }}""" )) ~>
        addHeader(RawHeader("x-auth-token", "legolas")) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[Command] === Command(
          inspect = Some(
            Inspect(
              Code(
                name        = "Bootstrap.java",
                description = "Resource Injector",
                content     = "class Bootstrap {void inject(Object object}{}}"
              )
            )
          ),
          None,
          None,
          None,
          None,
          None
        )
      }
    }


    "return an inspect request for POST requests to the root path" in {
      Post("/api/try", Command(inspect = Some(Inspect(Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object}{}}"))))) ~>
        addHeader(RawHeader("x-auth-token", "legolas")) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[Command] === Command(
          inspect = Some(
            Inspect(
              Code(
                name        = "Bootstrap.java",
                description = "Resource Injector",
                content     = "class Bootstrap {void inject(Object object}{}}"
              )
            )
          ),
          None,
          None,
          None,
          None,
          None
        )
      }
    }

    "return an inspect request with authorization token for POST requests to the root path" in {
      Post("/api/try?auth_token=legolas", Command(inspect = Some(Inspect(Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object}{}}"))))) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[Command] === Command(
          inspect = Some(
            Inspect(
              Code(
                name        = "Bootstrap.java",
                description = "Resource Injector",
                content     = "class Bootstrap {void inject(Object object}{}}"
              )
            )
          ),
          None,
          None,
          None,
          None,
          None
        )
      }
    }

    "return an inspect request with authorization token for PUT requests to the root path" in {
      Put("/api/try?auth_token=legolas", Command(inspect = Some(Inspect(Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object}{}}"))))) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[Command] === Command(
          inspect = Some(
            Inspect(
              Code(
                name        = "Bootstrap.java",
                description = "Resource Injector",
                content     = "class Bootstrap {void inject(Object object}{}}"
              )
            )
          ),
          None,
          None,
          None,
          None,
          None
        )
      }
    }

    "return a remove request with authorization token for POST requests with authorization token to the root path" in {
      Post("/api/try?auth_token=legolas", Command(remove = Some(Remove(what = "class", where = List(1, 2), source = Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object}{}}"))))) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[Command] === Command(
          None,
          remove = Some(
            Remove(
              what   = "class",
              where  = List(1, 2),
              source =
                Code(
                  name        = "Bootstrap.java",
                  description = "Resource Injector",
                  content     = "class Bootstrap {void inject(Object object}{}}"
              )
            )
          ),
          None,
          None,
          None,
          None
        )
      }
    }


    "return a remove request in JSON form for POST requests containing authorization HTTP Header to the root path" in {
      Post("/api/try?c", HttpEntity(MediaTypes.`application/json`, """{"remove" : { "what": "class", "where": [1, 2], "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"class Bootstrap {void inject(Object object}{}"} }}""" )) ~>
        addHeader(RawHeader("x-auth-token", "legolas")) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[Command] === Command(
          None,
          Some(
            Remove(
              "class",
              List(1, 2),
              Code(
                None,
                "Bootstrap.java",
                "Resource Injector",
                "class Bootstrap {void inject(Object object}{}",
                None
              )
            )
          ),
          None,
          None,
          None,
          None
        )
      }
    }

    "return a rename request for POST requests with authorization token to the root path" in {
      Post("/api/try?auth_token=legolas", Command(None, None, Some(Rename("class", List(1, 2), "ResourceInjector", Code(None,"Bootstrap.java","Resource Injector","class Bootstrap {void inject(Object object}{}", None))))) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[Command] === Command(
          None, None,
          Some(
            Rename(
              "class",
              List(1, 2),
              "ResourceInjector",
              Code(
                None,
                "Bootstrap.java",
                "Resource Injector",
                "class Bootstrap {void inject(Object object}{}",
                None
              )
            )
          ),
          None,
          None,
          None
        )
      }
    }


    "return a rename request in JSON form for POST requests with authorization token to the root path" in {
      Post("/api/try?auth_token=legolas", HttpEntity(MediaTypes.`application/json`, """{"rename" : { "what": "class", "where": [1, 2], "to":"ResourceInjector", "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"class Bootstrap {void inject(Object object}{}"} }}""" )) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[Command] === Command(
          None, None,
          Some(
            Rename(
              "class",
              List(1, 2),
              "ResourceInjector",
              Code(
                None,
                "Bootstrap.java",
                "Resource Injector",
                "class Bootstrap {void inject(Object object}{}",
                None
              )
            )
          ),
          None,
          None,
          None
        )
      }
    }


    "return an optimize request in JSON form for POST requests with authentication header to the root path" in {
      Post("/api/try?c", HttpEntity(MediaTypes.`application/json`, """{"optimize": { "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"class Bootstrap {void inject(Object object}{}}"} }}""" )) ~>
        addHeader(RawHeader("x-auth-token", "legolas")) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[Command] === Command(
          None, None, None,
          optimize = Some(
            Optimize(
              Code(
                name        = "Bootstrap.java",
                description = "Resource Injector",
                content     = "class Bootstrap {void inject(Object object}{}}"
              )
            )
          ),
          None,
          None
        )
      }
    }

    "return an optimize request for POST requests with authentication token to the root path" in {
      Post("/api/try?auth_token=legolas", Command(optimize = Some(Optimize(Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object}{}}"))))) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[Command] === Command(
          None, None, None,
          optimize = Some(
            Optimize(
              Code(
                name        = "Bootstrap.java",
                description = "Resource Injector",
                content     = "class Bootstrap {void inject(Object object}{}}"
              )
            )
          ),
          None,
          None
        )
      }
    }

    "return a format request for POST requests with authentication token to the root path" in {
      Post("/api/try?auth_token=legolas", Command(format = Some(Format(Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object}{}}"))))) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[Command] === Command(
          None, None, None, None,
          format = Some(
            Format(
              Code(
                name        = "Bootstrap.java",
                description = "Resource Injector",
                content     = "class Bootstrap {void inject(Object object}{}}"
              )
            )
          ),
          None
        )
      }
    }


    "return a deduplicate request for POST requests with authentication token to the root path" in {
      Post("/api/try?auth_token=legolas", Command(deduplicate = Some(Deduplicate(Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object}{}}"))))) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[Command] === Command(
          None, None, None, None, None,
          deduplicate = Some(
            Deduplicate(
              Code(
                name        = "Bootstrap.java",
                description = "Resource Injector",
                content     = "class Bootstrap {void inject(Object object}{}}"
              )
            )
          )
        )
      }
    }

  }

}
