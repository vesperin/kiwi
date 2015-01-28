package com.vesperin.kiwi.api

import com.vesperin.kiwi.domain.{Result, Code, Command, Stage, Multistage}
import org.specs2.mutable.Specification
import spray.testkit.Specs2RouteTest

import scala.concurrent.duration._

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
class PostKiwiMultistageApiSpec extends Specification with Specs2RouteTest with Kiwi {
  def actorRefFactory = system

  implicit val routeTestTimeout = RouteTestTimeout(FiniteDuration(2, SECONDS))

  "Kiwi" should {
    "Return a multistage request containing a list of imports for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command( multistage = Some(Multistage(source = Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(List<Object> object){}}"))))) ~>
        sealRoute(routes) ~> check {

        responseAs[Result].stages.get.stages.isEmpty === false
        responseAs[Result].stages.get.stages(0) mustEqual Stage("Inject", "inject", isBase = true, List(), Code("Bootstrap.java", "Resource Injector", "class Bootstrap {\n  void inject(List<Object> object) {}\n}"), budget = 15)
      }
    }

    "Return a multistage request (with preprocessing) containing a list of imports for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command( multistage = Some(Multistage(source = Code(name = "Scratched.java", description = "Java: Scratched", content = "private static Random rand = new Random();\n\npublic static void quicksort(int[] arr, int left, int right)\n{\n\t\t\tif (left < right)\n\t\t\t{\n\t\t\t\t\tint pivot = randomizedPartition(arr, left, right);\n\t\t\t\t\tquicksort(arr, left, pivot);\n\t\t\t\t\tquicksort(arr, pivot + 1, right);\n\t\t\t}\n}\n\nprivate static int randomizedPartition(int[] arr, int left, int right)\n{\n\t\t\tint swapIndex = left + rand.nextInt(right - left) + 1;\n\t\t\tswap(arr, left, swapIndex);\n\t\t\treturn partition(arr, left, right);\n}\n\nprivate static int partition(int[] arr, int left, int right)\n{\n\t\t\tint pivot = arr[left];\n\t\t\tint i = left - 1;\n\t\t\tint j = right + 1;\n\t\t\twhile (true)\n\t\t\t{\n\t\t\t\t\tdo\n\t\t\t\t\t\t\t\tj--;\n\t\t\t\t\twhile (arr[j] > pivot);\n\n\t\t\t\t\tdo\n\t\t\t\t\t\t\t\ti++;\n\t\t\t\t\twhile (arr[i] < pivot);\n\n\t\t\t\t\tif (i < j)\n\t\t\t\t\t\t\t\tswap(arr, i, j);\n\t\t\t\t\telse\n\t\t\t\t\t\t\t\treturn j;\n\t\t\t}\n}\n\nprivate static void swap(int[] arr, int i, int j)\n{\n\t\t\tint tmp = arr[i];\n\t\t\tarr[i] = arr[j];\n\t\t\tarr[j] = tmp;\n}\n\n\npublic static void main(String[] args)\n{\n\t\t\tint arr[] = new int[100000];\n\t\t\tfor (int i = 0; i < arr.length; i++)\n\t\t\t\t\tarr[i] = arr.length - i;\n\n\t\t\tSystem.out.println(\"First 20 elements\");\n\t\t\tSystem.out.print(\"Before sort: \");\n\t\t\tfor (int i = 0; i < 20; i++)\n\t\t\t\t\tSystem.out.print(arr[i] + \" \");\n\t\t\tSystem.out.println();\n\n\t\t\tquicksort(arr, 0, arr.length - 1);\n\t\t\tSystem.out.print(\"After sort: \");\n\t\t\tfor (int i = 0; i < 20; i++)\n\t\t\t\t\tSystem.out.print(arr[i] + \" \");\n\t\t\tSystem.out.println();\n}"), preprocess = true)))) ~>
        sealRoute(routes) ~> check {

        responseAs[Result].stages.get.stages.isEmpty === false
        responseAs[Result].stages.get.stages contains Stage("Partition", "partition", isBase = false, List(List(151, 328)), Code("Scratched.java", "Java: Scratched", "private static int partition(int[] arr, int left, int right)\n{\n\t\t\tint pivot = arr[left];\n\t\t\tint i = left - 1;\n\t\t\tint j = right + 1;\n\t\t\twhile (true)\n\t\t\t{\n\t\t\t\t\tdo\n\t\t\t\t\t\t\t\tj--;\n\t\t\t\t\twhile (arr[j] > pivot);\n\n\t\t\t\t\tdo\n\t\t\t\t\t\t\t\ti++;\n\t\t\t\t\twhile (arr[i] < pivot);\n\n\t\t\t\t\tif (i < j)\n\t\t\t\t\t\t\t\tswap(arr, i, j);\n\t\t\t\t\telse\n\t\t\t\t\t\t\t\treturn j;\n\t\t\t}\n}\n\nprivate static void swap(int[] arr, int i, int j)\n{\n\t\t\tint tmp = arr[i];\n\t\t\tarr[i] = arr[j];\n\t\t\tarr[j] = tmp;\n}"), budget = 15)
      }
    }
  }
}
