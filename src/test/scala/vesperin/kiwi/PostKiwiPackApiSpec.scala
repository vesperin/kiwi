package vesperin.kiwi

import org.specs2.mutable.Specification
import spray.testkit.Specs2RouteTest
import vesperin.kiwi.domain._
import vesperin.kiwi.routes.Kiwi

import scala.concurrent.duration._

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
class PostKiwiPackApiSpec extends Specification with Specs2RouteTest with Kiwi {
  def actorRefFactory = system

  implicit val routeTestTimeout = RouteTestTimeout(FiniteDuration(5, SECONDS))

  "Kiwi" should {
    "Return a pack request with missing imports in code example for POST requests to the root " +
      "path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(pack = Some(Pack(source =
        Code(id = "new", name = "Bootstrap.java", description = "Resource Injector",
          content = "class Bootstrap {void inject(List<Object> object){}}"),
        preprocess = false, imports = true)))
      ) ~>
        sealRoute(routes) ~> check {

        responseAs[Result].draft.get.after.content mustEqual "import java.util.List;\n\nclass Bootstrap {\n  void inject(List<Object> object) {}\n}"
      }
    }

    "Return a pack request w/o missing imports in code example for POST requests to the root " +
      "path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(pack = Some(Pack(source =
        Code(id = "new", name = "Bootstrap.java", description = "Resource Injector",
          content = "import java.util.List;\n\nclass Bootstrap {\n  void inject(List<Object> object) {}\n}"),
        preprocess = false, imports = true)))
      ) ~>
        sealRoute(routes) ~> check {

        responseAs[Result].draft.get.after.content mustEqual "import java.util.List;\n\nclass Bootstrap {\n  void inject(List<Object> object) {}\n}"
      }
    }

    "Return a pack request w/missing imports in partial code example for POST requests to " +
      "the " + "root " + "path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(pack = Some(Pack(source =
        Code(id = "new", name = "Scratched.java", description = "Scratched class",
          content = "void inject(List<Object> object) {}"),
        preprocess = true, imports = true)))
      ) ~>
        sealRoute(routes) ~> check {
        responseAs[Result].draft.get.after.content mustEqual "import java.util.List;\n\nclass Scratched {\n  void inject(List<Object> object) {}\n}"
      }
    }
  }

}
