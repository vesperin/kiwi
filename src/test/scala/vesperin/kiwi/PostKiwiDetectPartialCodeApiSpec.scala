package vesperin.kiwi

import vesperin.kiwi.domain._
import org.specs2.mutable.Specification
import spray.testkit.Specs2RouteTest
import vesperin.kiwi.routes.Kiwi

import scala.concurrent.duration._

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
class PostKiwiDetectPartialCodeApiSpec extends Specification with Specs2RouteTest with Kiwi {
  def actorRefFactory = system

  // reason of this addition? see https://groups.google.com/forum/#!msg/spray-user/o8DtI6VUMbA/n9tguTb_1noJ
  implicit val routeTestTimeout = RouteTestTimeout(FiniteDuration(5, SECONDS))


  "Kiwi" should {
    "Return a preprocess (true) request for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(preprocess = Some(Preprocess(Code(id = "new", name = "Scratched.java", description = "Scratched class", content = "public static  void main(String[] args) {\n    int []arr={12,23,43,34,3,6,7,1,9,6};\n        {  \n              int temp;\n              for (int i=0;i<arr.length;i++)\n              {  \n                for (int j=0;j<arr.length-i;j++ )\n                {\n                  if (arr[j]>arr[j+1])\n                 {  \n                     temp=arr[j];\n                     arr[j+1]=arr[j];\n                     arr[j+1]=temp;\n                  }\n                }\n              } \n            }\n        for(int i=0; i<arr.length; i++)\n         {\n             System.out.print(arr[i] + \" \");\n         }\n    }"))))) ~>
        sealRoute(routes) ~> check {

        responseAs[Result] mustEqual Result(info = Some(Info(List("true"))))
      }
    }

    "Return a preprocess (false) request for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(preprocess = Some(Preprocess(Code(id = "new", name = "Quicksort.java", description = "Quicksort class", content = "import java.util.Random;\n\npublic class Quicksort\n{\n     private static Random rand = new Random();\n\n     public static void quicksort(int[] arr, int left, int right)\n     {\n          if (left < right)\n          {\n               int pivot = randomizedPartition(arr, left, right);\n               quicksort(arr, left, pivot);\n               quicksort(arr, pivot + 1, right);\n          }\n     }\n\n     private static int randomizedPartition(int[] arr, int left, int right)\n     {\n          int swapIndex = left + rand.nextInt(right - left) + 1;\n          swap(arr, left, swapIndex);\n          return partition(arr, left, right);\n     }\n\n     private static int partition(int[] arr, int left, int right)\n     {\n          int pivot = arr[left];\n          int i = left - 1;\n          int j = right + 1;\n          while (true)\n          {\n               do\n                    j--;\n               while (arr[j] > pivot);\n\n               do\n                    i++;\n               while (arr[i] < pivot);\n\n               if (i < j)\n                    swap(arr, i, j);\n               else\n                    return j;\n          }\n     }\n\n     private static void swap(int[] arr, int i, int j)\n     {\n          int tmp = arr[i];\n          arr[i] = arr[j];\n          arr[j] = tmp;\n     }\n\n     // Sort 100k elements that are in reversed sorted order\n     public static void main(String[] args)\n     {\n          int arr[] = new int[100000];\n          for (int i = 0; i < arr.length; i++)\n               arr[i] = arr.length - i;\n\n          System.out.println(\"First 20 elements\");\n          System.out.print(\"Before sort: \");\n          for (int i = 0; i < 20; i++)\n               System.out.print(arr[i] + \" \");\n          System.out.println();\n\n          quicksort(arr, 0, arr.length - 1);\n          System.out.print(\"After sort: \");\n          for (int i = 0; i < 20; i++)\n               System.out.print(arr[i] + \" \");\n          System.out.println();\n     }\n\n}"))))) ~>
        sealRoute(routes) ~> check {

        responseAs[Result] mustEqual Result(info = Some(Info(List("false"))))
      }
    }
  }
}