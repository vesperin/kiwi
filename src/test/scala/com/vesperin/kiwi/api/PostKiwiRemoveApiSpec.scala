package com.vesperin.kiwi.api

import java.util.concurrent.TimeUnit._

import com.vesperin.kiwi.domain._
import org.specs2.mutable.Specification
import spray.http.{HttpEntity, MediaTypes}
import spray.testkit.Specs2RouteTest

import scala.concurrent.duration.Duration

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
class PostKiwiRemoveApiSpec extends Specification with Specs2RouteTest with Kiwi {
  def actorRefFactory = system

  implicit  val routeTestTimeout = RouteTestTimeout(Duration(5, SECONDS))

  "Kiwi" should {
    "return a remove method request in JSON form for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", HttpEntity(MediaTypes.`application/json`, """{"remove": { "what": "method", "where":[17, 45], "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"class Bootstrap {void inject(Object object){}}", "tags":[], "datastructures": [], "algorithms": [], "refactorings": [], "confidence": 2, "comments":[]}, "preprocess": false}}""" )) ~>
        sealRoute(routes) ~> check {
        responseAs[Result].draft.get.before mustEqual  Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}")
        responseAs[Result].draft.get.after  mustEqual  Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {\n}")
      }
    }

    "return a remove method (with preprocessing) request in JSON form for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", HttpEntity(MediaTypes.`application/json`, """{"remove": { "what": "method", "where":[939, 943], "source": {"name": "Scratched.java", "description":"Quicksort java implementation", "content":"private static Random rand = new Random();\n\npublic static void quicksort(int[] arr, int left, int right)\n{\n\t\t\tif (left < right)\n\t\t\t{\n\t\t\t\t\tint pivot = randomizedPartition(arr, left, right);\n\t\t\t\t\tquicksort(arr, left, pivot);\n\t\t\t\t\tquicksort(arr, pivot + 1, right);\n\t\t\t}\n}\n\nprivate static int randomizedPartition(int[] arr, int left, int right)\n{\n\t\t\tint swapIndex = left + rand.nextInt(right - left) + 1;\n\t\t\tswap(arr, left, swapIndex);\n\t\t\treturn partition(arr, left, right);\n}\n\nprivate static int partition(int[] arr, int left, int right)\n{\n\t\t\tint pivot = arr[left];\n\t\t\tint i = left - 1;\n\t\t\tint j = right + 1;\n\t\t\twhile (true)\n\t\t\t{\n\t\t\t\t\tdo\n\t\t\t\t\t\t\t\tj--;\n\t\t\t\t\twhile (arr[j] > pivot);\n\n\t\t\t\t\tdo\n\t\t\t\t\t\t\t\ti++;\n\t\t\t\t\twhile (arr[i] < pivot);\n\n\t\t\t\t\tif (i < j)\n\t\t\t\t\t\t\t\tswap(arr, i, j);\n\t\t\t\t\telse\n\t\t\t\t\t\t\t\treturn j;\n\t\t\t}\n}\n\nprivate static void swap(int[] arr, int i, int j)\n{\n\t\t\tint tmp = arr[i];\n\t\t\tarr[i] = arr[j];\n\t\t\tarr[j] = tmp;\n}\n\n\npublic static void main(String[] args)\n{\n\t\t\tint arr[] = new int[100000];\n\t\t\tfor (int i = 0; i < arr.length; i++)\n\t\t\t\t\tarr[i] = arr.length - i;\n\n\t\t\tSystem.out.println(\"First 20 elements\");\n\t\t\tSystem.out.print(\"Before sort: \");\n\t\t\tfor (int i = 0; i < 20; i++)\n\t\t\t\t\tSystem.out.print(arr[i] + \" \");\n\t\t\tSystem.out.println();\n\n\t\t\tquicksort(arr, 0, arr.length - 1);\n\t\t\tSystem.out.print(\"After sort: \");\n\t\t\tfor (int i = 0; i < 20; i++)\n\t\t\t\t\tSystem.out.print(arr[i] + \" \");\n\t\t\tSystem.out.println();\n}", "tags":[], "datastructures": [], "algorithms": [], "refactorings": [], "confidence": 2, "comments":[]}, "preprocess": true}}""" )) ~>
        sealRoute(routes) ~> check {

        responseAs[Result].draft.get.before mustEqual  Code(name = "Scratched.java", description = "Quicksort java implementation", content = "private static Random rand = new Random();\n\npublic static void quicksort(int[] arr, int left, int right)\n{\n\t\t\tif (left < right)\n\t\t\t{\n\t\t\t\t\tint pivot = randomizedPartition(arr, left, right);\n\t\t\t\t\tquicksort(arr, left, pivot);\n\t\t\t\t\tquicksort(arr, pivot + 1, right);\n\t\t\t}\n}\n\nprivate static int randomizedPartition(int[] arr, int left, int right)\n{\n\t\t\tint swapIndex = left + rand.nextInt(right - left) + 1;\n\t\t\tswap(arr, left, swapIndex);\n\t\t\treturn partition(arr, left, right);\n}\n\nprivate static int partition(int[] arr, int left, int right)\n{\n\t\t\tint pivot = arr[left];\n\t\t\tint i = left - 1;\n\t\t\tint j = right + 1;\n\t\t\twhile (true)\n\t\t\t{\n\t\t\t\t\tdo\n\t\t\t\t\t\t\t\tj--;\n\t\t\t\t\twhile (arr[j] > pivot);\n\n\t\t\t\t\tdo\n\t\t\t\t\t\t\t\ti++;\n\t\t\t\t\twhile (arr[i] < pivot);\n\n\t\t\t\t\tif (i < j)\n\t\t\t\t\t\t\t\tswap(arr, i, j);\n\t\t\t\t\telse\n\t\t\t\t\t\t\t\treturn j;\n\t\t\t}\n}\n\nprivate static void swap(int[] arr, int i, int j)\n{\n\t\t\tint tmp = arr[i];\n\t\t\tarr[i] = arr[j];\n\t\t\tarr[j] = tmp;\n}\n\n\npublic static void main(String[] args)\n{\n\t\t\tint arr[] = new int[100000];\n\t\t\tfor (int i = 0; i < arr.length; i++)\n\t\t\t\t\tarr[i] = arr.length - i;\n\n\t\t\tSystem.out.println(\"First 20 elements\");\n\t\t\tSystem.out.print(\"Before sort: \");\n\t\t\tfor (int i = 0; i < 20; i++)\n\t\t\t\t\tSystem.out.print(arr[i] + \" \");\n\t\t\tSystem.out.println();\n\n\t\t\tquicksort(arr, 0, arr.length - 1);\n\t\t\tSystem.out.print(\"After sort: \");\n\t\t\tfor (int i = 0; i < 20; i++)\n\t\t\t\t\tSystem.out.print(arr[i] + \" \");\n\t\t\tSystem.out.println();\n}")
        responseAs[Result].draft.get.after  mustEqual  Code(name = "Scratched.java", description = "Quicksort java implementation", content = "private static Random rand = new Random();\n\npublic static void quicksort(int[] arr, int left, int right)\n{\n\t\t\tif (left < right)\n\t\t\t{\n\t\t\t\t\tint pivot = randomizedPartition(arr, left, right);\n\t\t\t\t\tquicksort(arr, left, pivot);\n\t\t\t\t\tquicksort(arr, pivot + 1, right);\n\t\t\t}\n}\n\nprivate static int randomizedPartition(int[] arr, int left, int right)\n{\n\t\t\tint swapIndex = left + rand.nextInt(right - left) + 1;\n\t\t\tswap(arr, left, swapIndex);\n\t\t\treturn partition(arr, left, right);\n}\n\nprivate static int partition(int[] arr, int left, int right)\n{\n\t\t\tint pivot = arr[left];\n\t\t\tint i = left - 1;\n\t\t\tint j = right + 1;\n\t\t\twhile (true)\n\t\t\t{\n\t\t\t\t\tdo\n\t\t\t\t\t\t\t\tj--;\n\t\t\t\t\twhile (arr[j] > pivot);\n\n\t\t\t\t\tdo\n\t\t\t\t\t\t\t\ti++;\n\t\t\t\t\twhile (arr[i] < pivot);\n\n\t\t\t\t\tif (i < j)\n\t\t\t\t\t\t\t\tswap(arr, i, j);\n\t\t\t\t\telse\n\t\t\t\t\t\t\t\treturn j;\n\t\t\t}\n}\n\nprivate static void swap(int[] arr, int i, int j)\n{\n\t\t\tint tmp = arr[i];\n\t\t\tarr[i] = arr[j];\n\t\t\tarr[j] = tmp;\n}")
      }
    }

    "return a remove method request for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(remove = Some(Remove("method", where =  List(17, 45), source = Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}"))))) ~>
        sealRoute(routes) ~> check {
        responseAs[Result].draft.get.before mustEqual  Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}")
        responseAs[Result].draft.get.after  mustEqual  Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {\n}")
      }
    }

    "return a remove region request for POST requests to the root path (Web Example)" in {
      Post("/kiwi/eval?auth_token=legolas", Command(remove = Some(Remove("region", where =  List(68, 117), source = Code(name = "BubbleSort.java", description = "BubbleSort", content = "class BubbleSort {\n" +
        "\tpublic static void main(String[] arguments) {\n" +
        "\t\tint[] arr = { 12, 23, 43, 34, 3, 6, 7, 1, 9, 6 };\n" +
        "\t}\n" +
        "}"))))) ~>
        sealRoute(routes) ~> check {
        responseAs[Result].draft.get.after mustEqual Code(name = "BubbleSort.java", description = "BubbleSort", content = "class BubbleSort {\n  public static void main(String[] arguments) {}\n}")
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
