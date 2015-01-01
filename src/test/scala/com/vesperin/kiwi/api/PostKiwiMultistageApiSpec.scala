package com.vesperin.kiwi.api

import com.vesperin.kiwi.domain.{Result, Code, Command, Stage, Multistage}
import org.specs2.mutable.Specification
import spray.testkit.Specs2RouteTest

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
class PostKiwiMultistageApiSpec extends Specification with Specs2RouteTest with Kiwi {
  def actorRefFactory = system

  "Kiwi" should {
    "Return a multistage request containing a list of imports for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command( multistage = Some(Multistage(source = Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(List<Object> object){}}"))))) ~>
        sealRoute(routes) ~> check {

        responseAs[Result].stages.get.stages.isEmpty === false
        responseAs[Result].stages.get.stages(0) mustEqual Stage("Inject", "inject", isBase = true, List(), Code("Bootstrap.java", "Resource Injector", "class Bootstrap {\n  void inject(List<Object> object) {}\n}"), budget = 15)
      }
    }
  }
}
