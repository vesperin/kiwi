package edu.ucsc.vesper.http.api

import edu.ucsc.vesper.http.domain._
import org.specs2.mutable.Specification
import spray.http.{HttpEntity, MediaTypes}
import spray.testkit.Specs2RouteTest

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
class PostKiwiRemoveApiSpec extends Specification with Specs2RouteTest with Kiwi {
  def actorRefFactory = system

  "Kiwi" should {
    "return a remove method request in JSON form for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", HttpEntity(MediaTypes.`application/json`, """{"remove": { "what": "method", "where":[17, 45], "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"class Bootstrap {void inject(Object object){}}", "tags":[], "datastructures": [], "algorithms": [], "refactorings": [], "confidence": 2, "comments":[]} }}""" )) ~>
        sealRoute(routes) ~> check {
        responseAs[Result].draft.get.before mustEqual  Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}")
        responseAs[Result].draft.get.after  mustEqual  Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {}")
      }
    }

    "return a remove method request for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(remove = Some(Remove("method", where =  List(17, 45), source = Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}"))))) ~>
        sealRoute(routes) ~> check {
        responseAs[Result].draft.get.before mustEqual  Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}")
        responseAs[Result].draft.get.after  mustEqual  Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {}")
      }
    }

    "return a remove region request for POST requests to the root path (Web Example)" in {
      Post("/kiwi/eval?auth_token=legolas", Command(remove = Some(Remove("region", where =  List(68, 117), source = Code(name = "BubbleSort.java", description = "BubbleSort", content = "class BubbleSort {\n" +
        "\tpublic static void main(String[] arguments) {\n" +
        "\t\tint[] arr = { 12, 23, 43, 34, 3, 6, 7, 1, 9, 6 };\n" +
        "\t}\n" +
        "}"))))) ~>
        sealRoute(routes) ~> check {
        responseAs[Result].draft.get.after mustEqual Code(name = "BubbleSort.java", description = "BubbleSort", content = "class BubbleSort {\n\tpublic static void main(String[] arguments) {\n\t}\n}")
      }
    }

    "return a failed 'remove region request' for POST requests to the root path (Web 2 Example)" in {
      Post("/kiwi/eval?auth_token=legolas", Command(remove = Some(Remove("region", where =  List(73,122), source = Code(name = "ScratchedCodeSnippet.java", description = "ScratchedCodeSnippet", content = "class ScratchedCodeSnippet {\n\tpublic static void main(String[] args) {\n\t\tint[] arr = { 12, 23, 43, 34, 3, 6, 7, 1, 9, 6 };\n\t\t{\n\t\t\tint temp;\n\t\t\tfor (int i = 0; i < arr.length; i++) {\n\t\t\t\tfor (int j = 0; j < arr.length - i; j++) {\n\t\t\t\t\tif (arr[j] > arr[j + 1]) {\n\t\t\t\t\t\ttemp = arr[j];\n\t\t\t\t\t\tarr[j + 1] = arr[j];\n\t\t\t\t\t\tarr[j + 1] = temp;\n\t\t\t\t\t}\n\t\t\t\t}\n\t\t\t}\n\t\t}\n\t\tfor (int i = 0; i < arr.length; i++) {\n\t\t\tSystem.out.print(arr[i] + \" \");\n\t\t}\n\t}\n}"))))) ~>
        sealRoute(routes) ~> check {
        responseAs[Result].failure.get mustEqual Failure("arr cannot be deleted. It is referenced within the current scope!")
      }
    }
  }
}
