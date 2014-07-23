package edu.ucsc.vesper.http.api

import edu.ucsc.vesper.http.domain.Models._
import org.specs2.mutable.Specification
import spray.http.HttpHeaders.RawHeader
import spray.http.{HttpEntity, MediaTypes, StatusCodes}
import spray.testkit.Specs2RouteTest

import scala.concurrent.duration._

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
class FailingTests extends Specification with Specs2RouteTest with Vesperin {
  def actorRefFactory = system

  // reason of this addition? see https://groups.google.com/forum/#!msg/spray-user/o8DtI6VUMbA/n9tguTb_1noJ
  implicit val routeTestTimeout = RouteTestTimeout(FiniteDuration(15, SECONDS))

  "Vesperin" should {

    "return a cleanup request 1 for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(cleanup = Some(Cleanup(Code(name = "ToSort.java", description = "Name class", content = "import java.util.ArrayList;\nimport java.util.Collections;\nimport java.util.List;\nimport java.util.*;\n\npublic class ToSort implements Comparable {\n\n    private Float val;\n    private String id;\n\n    public ToSort(Float val, String id) {\n      this.val = val;\n      this.id = id;\n    }\n\n    @Override\n    public int compareTo(Object o) {\n\n      ToSort f = (ToSort) o;\n\n      if (val.floatValue() > f.val.floatValue()) {\n        return 1;\n      } else if (val.floatValue() < f.val.floatValue()) {\n        return -1;\n      } else {\n        return 0;\n      }\n\n    }\n\n    @Override\n    public String toString() {\n      return this.id;\n    }\n  }"))))) ~>
        sealRoute(vesperRoutes) ~> check {

        responseAs[Result].draft.get.before === Code(name = "ToSort.java", description = "Name class", content = "import java.util.ArrayList;\nimport java.util.Collections;\nimport java.util.List;\nimport java.util.*;\n\npublic class ToSort implements Comparable {\n\n    private Float val;\n    private String id;\n\n    public ToSort(Float val, String id) {\n      this.val = val;\n      this.id = id;\n    }\n\n    @Override\n    public int compareTo(Object o) {\n\n      ToSort f = (ToSort) o;\n\n      if (val.floatValue() > f.val.floatValue()) {\n        return 1;\n      } else if (val.floatValue() < f.val.floatValue()) {\n        return -1;\n      } else {\n        return 0;\n      }\n\n    }\n\n    @Override\n    public String toString() {\n      return this.id;\n    }\n  }")
        responseAs[Result].draft.get.after  === Code(name = "ToSort.java", description = "Name class", content = "import java.util.*;\n\npublic class ToSort implements Comparable {\n\n  private Float val;\n  private String id;\n\n  public ToSort(Float val, String id) {\n    this.val = val;\n    this.id = id;\n  }\n\n  @Override\n  public int compareTo(Object o) {\n\n    ToSort f = (ToSort) o;\n\n    if (val.floatValue() > f.val.floatValue()) {\n      return 1;\n    } else if (val.floatValue() < f.val.floatValue()) {\n      return -1;\n    } else {\n      return 0;\n    }\n\n  }\n\n  @Override\n  public String toString() {\n    return this.id;\n  }\n}\n")
      }
    }

  }
}
