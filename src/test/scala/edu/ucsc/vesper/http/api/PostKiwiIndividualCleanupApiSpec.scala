package edu.ucsc.vesper.http.api

import edu.ucsc.vesper.http.domain._
import org.specs2.mutable.Specification
import spray.http.{HttpEntity, MediaTypes}
import spray.testkit.Specs2RouteTest

import scala.concurrent.duration._

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
class PostKiwiIndividualCleanupApiSpec extends Specification with Specs2RouteTest with Kiwi {
  def actorRefFactory = system

  // reason of this addition? see https://groups.google.com/forum/#!msg/spray-user/o8DtI6VUMbA/n9tguTb_1noJ
  implicit val routeTestTimeout = RouteTestTimeout(FiniteDuration(5, SECONDS))

  "Kiwi" should {

    "Return an optimize imports request in JSON form for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", HttpEntity(MediaTypes.`application/json`, """{"optimize": { "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"import java.util.List;\t\n\tclass Bootstrap {void inject(Object object){}}", "tags":[], "datastructures": [], "algorithms": [], "refactorings": [], "confidence": 2, "comments":[]} }}""" )) ~>
        sealRoute(routes) ~> check {
        responseAs[Result].draft.get.before mustEqual  Code(name = "Bootstrap.java", description = "Resource Injector", content = "import java.util.List;\t\n\tclass Bootstrap {void inject(Object object){}}")
        responseAs[Result].draft.get.after  mustEqual  Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {\n  void inject(Object object) {}\n}")
      }
    }

    "Return an optimize imports request for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(optimize = Some(Optimize(source = Code(name = "Bootstrap.java", description = "Resource Injector", content = "import java.util.List;\t\n\tclass Bootstrap {void inject(Object object){}}"))))) ~>
        sealRoute(routes) ~> check {
        responseAs[Result].draft.get.before mustEqual  Code(name = "Bootstrap.java", description = "Resource Injector", content = "import java.util.List;\t\n\tclass Bootstrap {void inject(Object object){}}")
        responseAs[Result].draft.get.after  mustEqual  Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {\n  void inject(Object object) {}\n}")
      }
    }

    "Return a formatting request for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(format = Some(Format(Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}"))))) ~>
        sealRoute(routes) ~> check {
        responseAs[Result].draft.get.before mustEqual Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}")
        responseAs[Result].draft.get.after  mustEqual Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {\n  void inject(Object object) {}\n}")
      }
    }

    "Return a formatting request in JSON form for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", HttpEntity(MediaTypes.`application/json`, """{"format": { "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"class Bootstrap {void inject(Object object){}}", "tags":[], "datastructures": [], "algorithms": [], "refactorings": [], "confidence": 2, "comments":[]} }}""" )) ~>
        sealRoute(routes) ~> check {
        responseAs[Result].draft.get.before mustEqual Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}")
        responseAs[Result].draft.get.after  mustEqual Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {\n  void inject(Object object) {}\n}")
      }
    }

    "Return a deduplicate request for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(deduplicate = Some(Deduplicate(Code(name = "Name.java", description = "Name class", content = "class Name {\n\t/** {@link Name#boom(String)} **/\tvoid boom(){ System.out.println(1); }\n\tvoid baam(){ System.out.println(1); }\n\tvoid beem(){ System.out.println(1); }\n\tvoid buum(){ baam(); }\n}"))))) ~>
        sealRoute(routes) ~> check {
        responseAs[Result].draft.get.before === Code(name = "Name.java", description = "Name class", content = "class Name {\n\t/** {@link Name#boom(String)} **/\tvoid boom(){ System.out.println(1); }\n\tvoid baam(){ System.out.println(1); }\n\tvoid beem(){ System.out.println(1); }\n\tvoid buum(){ baam(); }\n}")
        responseAs[Result].draft.get.after  === Code(name = "Name.java", description = "Name class", content = "class Name {\n\t/** {@link Name#boom(String)} **/\tvoid boom(){ System.out.println(1); }\n\tvoid buum(){ boom(); }\n}")
      }
    }

    "Return a deduplicate request in JSON form for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", HttpEntity(MediaTypes.`application/json`, """{"deduplicate": { "source": {"name": "Name.java", "description":"Name class", "content":"class Name {\n\t/** {@link Name#boom(String)} **/\tvoid boom(){ System.out.println(1); }\n\tvoid baam(){ System.out.println(1); }\n\tvoid beem(){ System.out.println(1); }\n\tvoid buum(){ baam(); }\n}", "tags":[], "datastructures": [], "algorithms": [], "refactorings": [], "confidence": 2, "comments":[]} }}""" )) ~>
        sealRoute(routes) ~> check {
        responseAs[Result].draft.get.before mustEqual Code(name = "Name.java", description = "Name class", content = "class Name {\n\t/** {@link Name#boom(String)} **/\tvoid boom(){ System.out.println(1); }\n\tvoid baam(){ System.out.println(1); }\n\tvoid beem(){ System.out.println(1); }\n\tvoid buum(){ baam(); }\n}")
        responseAs[Result].draft.get.after  mustEqual Code(name = "Name.java", description = "Name class", content = "class Name {\n\t/** {@link Name#boom(String)} **/\tvoid boom(){ System.out.println(1); }\n\tvoid buum(){ boom(); }\n}")
      }
    }

    "Return a deduplicate request in JSON form for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", HttpEntity(MediaTypes.`application/json`, """{"deduplicate":{"source":{"name":"ScratchedCodeSnippet.java","description":"Java: *scratched* code snippet","content":"import java.util.*;\nimport java.lang.*;\n\nclass ScratchedCodeSnippet {\n  public static void main(String[] args) {\n    int[] arr = {12, 23, 43, 34, 3, 6, 7, 1, 9, 6};\n    {\n      int temp;\n      for (int i = 0; i < arr.length; i++) {\n        for (int j = 0; j < arr.length - i; j++) {\n          if (arr[j] > arr[j + 1]) {\n            temp = arr[j];\n            arr[j + 1] = arr[j];\n            arr[j + 1] = temp;\n          }\n        }\n      }\n    }\n    for (int i = 0; i < arr.length; i++) {\n      System.out.print(arr[i] + \" \");\n    }\n  }\n}\n","elapsedtime":"","tags":[],"datastructures":[],"algorithms":[],"refactorings":[],"confidence":0,"comments":[],"url":"http://stackoverflow.com/questions/12339939/bubble-sort-in-java","birthday":1406125099529}}}""" )) ~>
        sealRoute(routes) ~> check {
        responseAs[Result].info.get mustEqual Info(List("everything looks clear!"))
      }
    }
    
    "Return a trim method request (no dependencies) for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(trim = Some(Trim(source = Code(name = "ScratchedCodeSnippet.java", description = "Bubble sort implementation", content = "class ScratchedCodeSnippet {\n  public static void main(String[] args) {\n    int[] arr = {12, 23, 43, 34, 3, 6, 7, 1, 9, 6};\n    {\n      int temp;\n      for (int i = 0; i < arr.length; i++) {\n        for (int j = 0; j < arr.length - i; j++) {\n          if (arr[j] > arr[j + 1]) {\n            temp = arr[j];\n            arr[j + 1] = arr[j];\n            arr[j + 1] = temp;\n          }\n        }\n      }\n    }\n    for (int i = 0; i < arr.length; i++) {\n      System.out.print(arr[i] + \" \");\n    }\n  }\n}"), where =  List(31, 496))))) ~>
        sealRoute(routes) ~> check {
        responseAs[Result].draft.get.before mustEqual  Code(name = "ScratchedCodeSnippet.java", description = "Bubble sort implementation", content = "class ScratchedCodeSnippet {\n  public static void main(String[] args) {\n    int[] arr = {12, 23, 43, 34, 3, 6, 7, 1, 9, 6};\n    {\n      int temp;\n      for (int i = 0; i < arr.length; i++) {\n        for (int j = 0; j < arr.length - i; j++) {\n          if (arr[j] > arr[j + 1]) {\n            temp = arr[j];\n            arr[j + 1] = arr[j];\n            arr[j + 1] = temp;\n          }\n        }\n      }\n    }\n    for (int i = 0; i < arr.length; i++) {\n      System.out.print(arr[i] + \" \");\n    }\n  }\n}")
        responseAs[Result].draft.get.after  mustEqual  Code(name = "ScratchedCodeSnippet.java", description = "Bubble sort implementation", content = "class ScratchedCodeSnippet {\n  public static void main(String[] args) {\n    int[] arr = {12, 23, 43, 34, 3, 6, 7, 1, 9, 6};\n    {\n      int temp;\n      for (int i = 0; i < arr.length; i++) {\n        for (int j = 0; j < arr.length - i; j++) {\n          if (arr[j] > arr[j + 1]) {\n            temp = arr[j];\n            arr[j + 1] = arr[j];\n            arr[j + 1] = temp;\n          }\n        }\n      }\n    }\n    for (int i = 0; i < arr.length; i++) {\n      System.out.print(arr[i] + \" \");\n    }\n  }\n}")
      }
    }
    
    "Return a trim method (with dependencies) request for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(trim = Some(Trim(source = Code(name = "Name.java", description = "Name class implementation", content = "import java.util.List; \nimport java.util.Collection; \nclass Name {\nString msg = \"Hi!\";\n\tString boom(String msg){ if(null != msg) { return boom(null);} return \"Hi!\";}\n\t/** {@link Name#boom(String)}**/String baam(String msg){  return msg; }\nString beem(String text){ return boom(text); }}"), where =  List(88, 165))))) ~>
        sealRoute(routes) ~> check {
        responseAs[Result].draft.get.before mustEqual  Code(name = "Name.java", description = "Name class implementation", content = "import java.util.List; \nimport java.util.Collection; \nclass Name {\nString msg = \"Hi!\";\n\tString boom(String msg){ if(null != msg) { return boom(null);} return \"Hi!\";}\n\t/** {@link Name#boom(String)}**/String baam(String msg){  return msg; }\nString beem(String text){ return boom(text); }}")
        responseAs[Result].draft.get.after  mustEqual  Code(name = "Name.java", description = "Name class implementation", content = "import java.util.List;\nimport java.util.Collection;\n\nclass Name {\n  String boom(String msg) {\n    if (null != msg) {\n      return boom(null);\n    }\n    return \"Hi!\";\n  }\n}")
      }
    }

  }
}
