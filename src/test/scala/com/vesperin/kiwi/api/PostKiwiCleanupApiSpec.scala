package com.vesperin.kiwi.api

import com.vesperin.kiwi.domain._
import org.specs2.mutable.Specification
import spray.http.{HttpEntity, MediaTypes}
import spray.testkit.Specs2RouteTest

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
class PostKiwiCleanupApiSpec extends Specification with Specs2RouteTest with Kiwi {
  def actorRefFactory = system

  "Kiwi" should {
    "Return a cleanup request 1 for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(cleanup = Some(Cleanup(Code(name = "ToSort.java", description = "Name class", content = "import java.util.ArrayList;\nimport java.util.Collections;\nimport java.util.List;\nimport java.util.*;\n\npublic class ToSort implements Comparable {\n\n    private Float val;\n    private String id;\n\n    public ToSort(Float val, String id) {\n      this.val = val;\n      this.id = id;\n    }\n\n    @Override\n    public int compareTo(Object o) {\n\n      ToSort f = (ToSort) o;\n\n      if (val.floatValue() > f.val.floatValue()) {\n        return 1;\n      } else if (val.floatValue() < f.val.floatValue()) {\n        return -1;\n      } else {\n        return 0;\n      }\n\n    }\n\n    @Override\n    public String toString() {\n      return this.id;\n    }\n  }"))))) ~>
        sealRoute(routes) ~> check {

        responseAs[Result].draft.get.before === Code(name = "ToSort.java", description = "Name class", content = "import java.util.ArrayList;\nimport java.util.Collections;\nimport java.util.List;\nimport java.util.*;\n\npublic class ToSort implements Comparable {\n\n  private Float val;\n  private String id;\n\n  public ToSort(Float val, String id) {\n    this.val = val;\n    this.id = id;\n  }\n\n  @Override\n  public int compareTo(Object o) {\n\n    ToSort f = (ToSort) o;\n\n    if (val.floatValue() > f.val.floatValue()) {\n      return 1;\n    } else if (val.floatValue() < f.val.floatValue()) {\n      return -1;\n    } else {\n      return 0;\n    }\n\n  }\n\n  @Override\n  public String toString() {\n    return this.id;\n  }\n}")
        responseAs[Result].draft.get.after  === Code(name = "ToSort.java", description = "Name class", content = "import java.util.*;\n\npublic class ToSort implements Comparable {\n\n  private Float val;\n  private String id;\n\n  public ToSort(Float val, String id) {\n    this.val = val;\n    this.id = id;\n  }\n\n  @Override\n  public int compareTo(Object o) {\n\n    ToSort f = (ToSort) o;\n\n    if (val.floatValue() > f.val.floatValue()) {\n      return 1;\n    } else if (val.floatValue() < f.val.floatValue()) {\n      return -1;\n    } else {\n      return 0;\n    }\n\n  }\n\n  @Override\n  public String toString() {\n    return this.id;\n  }\n}")
      }
    }

    "Return a cleanup request for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(cleanup = Some(Cleanup(Code(name = "Name.java", description = "Name class", content = "class Name {\n\t/** {@link Name#boom(String)} **/\tvoid boom(){ System.out.println(1); }\n\tvoid baam(){ System.out.println(1); }\n\tvoid beem(){ System.out.println(1); }\n\tvoid buum(){ baam(); }\n}"))))) ~>
        sealRoute(routes) ~> check {
        responseAs[Result].draft.get.before === Code(name = "Name.java", description = "Name class", content = "class Name {\n  /** {@link Name#boom(String)} **/\n  void boom() {\n    System.out.println(1);\n  }\n\n  void baam() {\n    System.out.println(1);\n  }\n\n  void beem() {\n    System.out.println(1);\n  }\n\n  void buum() {\n    baam();\n  }\n}")
        responseAs[Result].draft.get.after  === Code(name = "Name.java", description = "Name class", content = "class Name {\n  /** {@link Name#boom(String)} **/\n  void boom() {\n    System.out.println(1);\n  }\n\n  void buum() {\n    boom();\n  }\n}")
      }
    }

    "Return a cleanup request in JSON form for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", HttpEntity(MediaTypes.`application/json`, """{"cleanup": { "source": {"name": "Name.java", "description":"Name class", "content":"class Name {\n\t/** {@link Name#boom(String)} **/\tvoid boom(){ System.out.println(1); }\n\tvoid baam(){ System.out.println(1); }\n\tvoid beem(){ System.out.println(1); }\n\tvoid buum(){ baam(); }\n}", "tags":[], "datastructures": [], "algorithms": [], "refactorings": [], "confidence": 2, "comments":[]}, "preprocess": false }}""" )) ~>
        sealRoute(routes) ~> check {
        responseAs[Result].draft.get.before === Code(name = "Name.java", description = "Name class", content = "class Name {\n  /** {@link Name#boom(String)} **/\n  void boom() {\n    System.out.println(1);\n  }\n\n  void baam() {\n    System.out.println(1);\n  }\n\n  void beem() {\n    System.out.println(1);\n  }\n\n  void buum() {\n    baam();\n  }\n}")
        responseAs[Result].draft.get.after  === Code(name = "Name.java", description = "Name class", content = "class Name {\n  /** {@link Name#boom(String)} **/\n  void boom() {\n    System.out.println(1);\n  }\n\n  void buum() {\n    boom();\n  }\n}")
      }
    }


  }

}
