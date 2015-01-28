package com.vesperin.kiwi.api

import com.vesperin.kiwi.domain.{Code, Command, Result, Stage, Summarize}
import org.specs2.mutable.Specification
import spray.testkit.Specs2RouteTest

import scala.concurrent.duration._

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
class PostKiwiSummarizeApiSpec extends Specification with Specs2RouteTest with Kiwi {
  def actorRefFactory = system

  implicit val routeTestTimeout = RouteTestTimeout(FiniteDuration(2, SECONDS))

  "Kiwi" should {
    "Return a summary request containing no foldable regions for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(summarize = Some(Summarize(stage = Stage("Inject", "inject", isBase = true, List(), Code("Bootstrap.java", "Resource Injector", "class Bootstrap {\n  void inject(List<Object> object) {}\n}"), budget = 2))))) ~>
        sealRoute(routes) ~> check {

        responseAs[Result].stage.get mustEqual Stage("Inject", "inject", isBase = true, List(), Code("Bootstrap.java", "Resource Injector", "class Bootstrap {\n  void inject(List<Object> object) {}\n}"), budget = 2)
      }
    }


    "Return a summary request containing the summarized stage for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(summarize = Some(Summarize(stage = Stage("Partition", "partition", isBase = false, List(), Code("Quicksort.java", "Java: Quicksort", "public class Quicksort {\n  private static int partition(int[] arr, int left, int right) {\n    int pivot = arr[left];\n    int i = left - 1;\n    int j = right + 1;\n    while (true) {\n      do\n        j--;\n      while (arr[j] > pivot);\n\n      do\n        i++;\n      while (arr[i] < pivot);\n\n      if (i < j)\n        swap(arr, i, j);\n      else\n        return j;\n    }\n  }\n\n  private static void swap(int[] arr, int i, int j) {\n    int tmp = arr[i];\n    arr[i] = arr[j];\n    arr[j] = tmp;\n  }\n\n}"), budget = 10))))) ~>
        sealRoute(routes) ~> check {
//        import spray.json._
//
//        println(responseAs[Result].stage.get.toJson)
        responseAs[Result].stage.get mustEqual Stage("Partition", "partition", isBase = false, List(List(179, 363)), Code("Quicksort.java", "Java: Quicksort", "public class Quicksort {\n  private static int partition(int[] arr, int left, int right) {\n    int pivot = arr[left];\n    int i = left - 1;\n    int j = right + 1;\n    while (true) {\n      do\n        j--;\n      while (arr[j] > pivot);\n\n      do\n        i++;\n      while (arr[i] < pivot);\n\n      if (i < j)\n        swap(arr, i, j);\n      else\n        return j;\n    }\n  }\n\n  private static void swap(int[] arr, int i, int j) {\n    int tmp = arr[i];\n    arr[i] = arr[j];\n    arr[j] = tmp;\n  }\n\n}"), budget = 10)
      }
    }


    "Return a summary request (with preprocessing) containing the summarized stage for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(summarize = Some(Summarize(stage = Stage("Partition", "partition", isBase = false, List(), Code("Quicksort.java", "Java: Quicksort", "private static int partition(int[] arr, int left, int right) {\n    int pivot = arr[left];\n    int i = left - 1;\n    int j = right + 1;\n    while (true) {\n      do\n        j--;\n      while (arr[j] > pivot);\n\n      do\n        i++;\n      while (arr[i] < pivot);\n\n      if (i < j)\n        swap(arr, i, j);\n      else\n        return j;\n    }\n  }\n\n  private static void swap(int[] arr, int i, int j) {\n    int tmp = arr[i];\n    arr[i] = arr[j];\n    arr[j] = tmp;\n  }"), budget = 10), preprocess = true)))) ~>
        sealRoute(routes) ~> check {
        responseAs[Result].stage.get mustEqual Stage("Partition", "partition", isBase = false, List(List(152, 336)), Code("Quicksort.java", "Java: Quicksort", "private static int partition(int[] arr, int left, int right) {\n    int pivot = arr[left];\n    int i = left - 1;\n    int j = right + 1;\n    while (true) {\n      do\n        j--;\n      while (arr[j] > pivot);\n\n      do\n        i++;\n      while (arr[i] < pivot);\n\n      if (i < j)\n        swap(arr, i, j);\n      else\n        return j;\n    }\n  }\n\n  private static void swap(int[] arr, int i, int j) {\n    int tmp = arr[i];\n    arr[i] = arr[j];\n    arr[j] = tmp;\n  }"), budget = 10)
      }
    }
  }
}
