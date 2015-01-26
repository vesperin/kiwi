package com.vesperin.kiwi.api

import com.vesperin.kiwi.domain._
import org.specs2.mutable.Specification
import spray.http.HttpHeaders.RawHeader
import spray.http.{HttpEntity, MediaTypes}
import spray.testkit.Specs2RouteTest

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
class PostKiwiRenameApiSpec extends Specification with Specs2RouteTest with Kiwi {
  def actorRefFactory = system

  "Kiwi" should {
    "Return a rename class request for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(rename = Some(Rename("class", where =  List(6, 15), to = "Preconditions", source = Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}"))))) ~>
        sealRoute(routes) ~> check {


        responseAs[Result].draft.get.before mustEqual  Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}")
        responseAs[Result].draft.get.after  mustEqual  Code(name = "Preconditions.java", description = "Resource Injector", content = "class Preconditions {\n  void inject(Object object) {}\n}")
      }
    }


    "Return a rename member request for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(rename = Some(Rename("member", where =  List(6, 15), to = "Preconditions", source = Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}"))))) ~>
        sealRoute(routes) ~> check {
        responseAs[Result].draft.get.before mustEqual  Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}")
        responseAs[Result].draft.get.after  mustEqual  Code(name = "Preconditions.java", description = "Resource Injector", content = "class Preconditions {\n  void inject(Object object) {}\n}")
      }
    }


    "Return a rename class request in JSON form for POST requests to the root path" in {
      Post("/kiwi/eval", HttpEntity(MediaTypes.`application/json`, """{"rename": { "what": "class", "where":[6, 15], "to": "Preconditions", "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"class Bootstrap {void inject(Object object){}}", "tags":[], "datastructures": [], "algorithms": [], "refactorings": [], "confidence": 2, "comments":[]}, "preprocess": true }}""" )) ~>
        addHeader(RawHeader("x-auth-token", "legolas")) ~>
        sealRoute(routes) ~> check {

        responseAs[Result].draft.get.before mustEqual  Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}")
        responseAs[Result].draft.get.after  mustEqual  Code(name = "Preconditions.java", description = "Resource Injector", content = "class Preconditions {\n  void inject(Object object) {}\n}")

      }
    }
  }
}
