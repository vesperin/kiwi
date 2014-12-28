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
class VesperinSpec extends Specification with Specs2RouteTest with Vesperin {
  def actorRefFactory = system

  // reason of this addition? see https://groups.google.com/forum/#!msg/spray-user/o8DtI6VUMbA/n9tguTb_1noJ
  implicit val routeTestTimeout = RouteTestTimeout(FiniteDuration(5, SECONDS))

  "Vesperin" should {
    "return a greeting for GET requests to the 'all' path" in {
      Get("/kiwi/help") ~> vesperRoutes ~> check {
        responseAs[String] must contain("Code Transformation Functionality")
      }
    }


    "return an inspect request for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(inspect = Some(Inspect(Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}"))))) ~>
        sealRoute(vesperRoutes) ~> check {

        responseAs[Result] mustEqual Result(warnings = Some(List()))
      }
    }

    "return an inspect request containing a warning for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(inspect = Some(Inspect(Code(name = "Random.java", description = "Lalalalal", content = "import java.util.PriorityQueue;\nimport java.util.List;\nimport java.util.ArrayList;\nimport java.util.Collections;\n\nclass Vertex implements Comparable<Vertex>\n{\n    public final String name;\n    public Edge[] adjacencies;\n    public double minDistance = Double.POSITIVE_INFINITY;\n    public Vertex previous;\n    public Vertex(String argName) { name = argName; }\n    public String toString() { return name; }\n    public int compareTo(Vertex other)\n    {\n        return Double.compare(minDistance, other.minDistance);\n    }\n\n}\n\n\nclass Edge\n{\n    public final Vertex target;\n    public final double weight;\n    public Edge(Vertex argTarget, double argWeight)\n    { target = argTarget; weight = argWeight; }\n}\n\npublic class Dijkstra\n{\n    public static void computePaths(Vertex source)\n    {\n        source.minDistance = 0.;\n        PriorityQueue<Vertex> vertexQueue = new PriorityQueue<Vertex>();\n    vertexQueue.add(source);\n\n    while (!vertexQueue.isEmpty()) {\n        Vertex u = vertexQueue.poll();\n\n            // Visit each edge exiting u\n            for (Edge e : u.adjacencies)\n            {\n                Vertex v = e.target;\n                double weight = e.weight;\n                double distanceThroughU = u.minDistance + weight;\n        if (distanceThroughU < v.minDistance) {\n            vertexQueue.remove(v);\n\n            v.minDistance = distanceThroughU ;\n            v.previous = u;\n            vertexQueue.add(v);\n        }\n            }\n        }\n    }\n\n    public static List<Vertex> getShortestPathTo(Vertex target)\n    {\n        List<Vertex> path = new ArrayList<Vertex>();\n        for (Vertex vertex = target; vertex != null; vertex = vertex.previous)\n            path.add(vertex);\n\n        Collections.reverse(path);\n        return path;\n    }\n\n    public static void main(String[] args)\n    {\n        // mark all the vertices \n        Vertex A = new Vertex(\"A\");\n        Vertex B = new Vertex(\"B\");\n        Vertex D = new Vertex(\"D\");\n        Vertex F = new Vertex(\"F\");\n        Vertex K = new Vertex(\"K\");\n        Vertex J = new Vertex(\"J\");\n        Vertex M = new Vertex(\"M\");\n        Vertex O = new Vertex(\"O\");\n        Vertex P = new Vertex(\"P\");\n        Vertex R = new Vertex(\"R\");\n        Vertex Z = new Vertex(\"Z\");\n\n        // set the edges and weight\n        A.adjacencies = new Edge[]{ new Edge(M, 8) };\n        B.adjacencies = new Edge[]{ new Edge(D, 11) };\n        D.adjacencies = new Edge[]{ new Edge(B, 11) };\n        F.adjacencies = new Edge[]{ new Edge(K, 23) };\n        K.adjacencies = new Edge[]{ new Edge(O, 40) };\n        J.adjacencies = new Edge[]{ new Edge(K, 25) };\n        M.adjacencies = new Edge[]{ new Edge(R, 8) };\n        O.adjacencies = new Edge[]{ new Edge(K, 40) };\n        P.adjacencies = new Edge[]{ new Edge(Z, 18) };\n        R.adjacencies = new Edge[]{ new Edge(P, 15) };\n        Z.adjacencies = new Edge[]{ new Edge(P, 18) };\n\n\n        computePaths(A); // run Dijkstra\n        System.out.println(\"Distance to \" + Z + \": \" + Z.minDistance);\n        List<Vertex> path = getShortestPathTo(Z);\n        System.out.println(\"Path: \" + path);\n    }\n}"))))) ~> sealRoute(vesperRoutes) ~> check {
        responseAs[Result] mustEqual Result(warnings = Some(List(Warning("The public type Dijkstra must be defined in its own file. Location(line=30, start=718, end=725)."))))
      }
    }

    "return an inspect request containing a list of imports for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(inspect = Some(Inspect(source = Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(List<Object> object){}}"), imports = true)))) ~>
        sealRoute(vesperRoutes) ~> check {

        responseAs[Result] mustEqual Result(info = Some(Info(List("java.util.List;"))))
      }
    }

    "return a rename class request for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(rename = Some(Rename("class", where =  List(6, 15), to = "Preconditions", source = Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}"))))) ~>
        sealRoute(vesperRoutes) ~> check {


        responseAs[Result].draft.get.before mustEqual  Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}")
        responseAs[Result].draft.get.after  mustEqual  Code(name = "Preconditions.java", description = "Resource Injector", content = "class Preconditions {\n  void inject(Object object) {}\n}")
      }
    }


    "return a rename member request for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(rename = Some(Rename("member", where =  List(6, 15), to = "Preconditions", source = Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}"))))) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[Result].draft.get.before mustEqual  Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}")
        responseAs[Result].draft.get.after  mustEqual  Code(name = "Preconditions.java", description = "Resource Injector", content = "class Preconditions {\n  void inject(Object object) {}\n}")
      }
    }


    "return a rename class request in JSON form for POST requests to the root path" in {
      Post("/kiwi/eval", HttpEntity(MediaTypes.`application/json`, """{"rename": { "what": "class", "where":[6, 15], "to": "Preconditions", "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"class Bootstrap {void inject(Object object){}}", "tags":[], "datastructures": [], "algorithms": [], "refactorings": [], "confidence": 2, "comments":[]} }}""" )) ~>
        addHeader(RawHeader("x-auth-token", "legolas")) ~>
        sealRoute(vesperRoutes) ~> check {

        responseAs[Result].draft.get.before mustEqual  Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}")
        responseAs[Result].draft.get.after  mustEqual  Code(name = "Preconditions.java", description = "Resource Injector", content = "class Preconditions {\n  void inject(Object object) {}\n}")

      }
    }

    "return a remove method request in JSON form for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", HttpEntity(MediaTypes.`application/json`, """{"remove": { "what": "method", "where":[17, 45], "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"class Bootstrap {void inject(Object object){}}", "tags":[], "datastructures": [], "algorithms": [], "refactorings": [], "confidence": 2, "comments":[]} }}""" )) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[Result].draft.get.before mustEqual  Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}")
        responseAs[Result].draft.get.after  mustEqual  Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {}")
      }
    }

    "return a remove method request for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(remove = Some(Remove("method", where =  List(17, 45), source = Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}"))))) ~>
        sealRoute(vesperRoutes) ~> check {
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
        sealRoute(vesperRoutes) ~> check {
        responseAs[Result].draft.get.after mustEqual Code(name = "BubbleSort.java", description = "BubbleSort", content = "class BubbleSort {\n\tpublic static void main(String[] arguments) {\n\t}\n}")
      }
    }

    "return a failed 'remove region request' for POST requests to the root path (Web 2 Example)" in {
      Post("/kiwi/eval?auth_token=legolas", Command(remove = Some(Remove("region", where =  List(73,122), source = Code(name = "ScratchedCodeSnippet.java", description = "ScratchedCodeSnippet", content = "class ScratchedCodeSnippet {\n\tpublic static void main(String[] args) {\n\t\tint[] arr = { 12, 23, 43, 34, 3, 6, 7, 1, 9, 6 };\n\t\t{\n\t\t\tint temp;\n\t\t\tfor (int i = 0; i < arr.length; i++) {\n\t\t\t\tfor (int j = 0; j < arr.length - i; j++) {\n\t\t\t\t\tif (arr[j] > arr[j + 1]) {\n\t\t\t\t\t\ttemp = arr[j];\n\t\t\t\t\t\tarr[j + 1] = arr[j];\n\t\t\t\t\t\tarr[j + 1] = temp;\n\t\t\t\t\t}\n\t\t\t\t}\n\t\t\t}\n\t\t}\n\t\tfor (int i = 0; i < arr.length; i++) {\n\t\t\tSystem.out.print(arr[i] + \" \");\n\t\t}\n\t}\n}"))))) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[Result].failure.get mustEqual Failure("arr cannot be deleted. It is referenced within the current scope!")
      }
    }

    "return an optimize imports request in JSON form for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", HttpEntity(MediaTypes.`application/json`, """{"optimize": { "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"import java.util.List;\t\n\tclass Bootstrap {void inject(Object object){}}", "tags":[], "datastructures": [], "algorithms": [], "refactorings": [], "confidence": 2, "comments":[]} }}""" )) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[Result].draft.get.before mustEqual  Code(name = "Bootstrap.java", description = "Resource Injector", content = "import java.util.List;\t\n\tclass Bootstrap {void inject(Object object){}}")
        responseAs[Result].draft.get.after  mustEqual  Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {\n  void inject(Object object) {}\n}")
      }
    }

    "return an optimize imports request for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(optimize = Some(Optimize(source = Code(name = "Bootstrap.java", description = "Resource Injector", content = "import java.util.List;\t\n\tclass Bootstrap {void inject(Object object){}}"))))) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[Result].draft.get.before mustEqual  Code(name = "Bootstrap.java", description = "Resource Injector", content = "import java.util.List;\t\n\tclass Bootstrap {void inject(Object object){}}")
        responseAs[Result].draft.get.after  mustEqual  Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {\n  void inject(Object object) {}\n}")
      }
    }

    "return a formatting request for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(format = Some(Format(Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}"))))) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[Result].draft.get.before mustEqual Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}")
        responseAs[Result].draft.get.after  mustEqual Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {\n  void inject(Object object) {}\n}")
      }
    }

    "return a formatting request in JSON form for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", HttpEntity(MediaTypes.`application/json`, """{"format": { "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"class Bootstrap {void inject(Object object){}}", "tags":[], "datastructures": [], "algorithms": [], "refactorings": [], "confidence": 2, "comments":[]} }}""" )) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[Result].draft.get.before mustEqual Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}")
        responseAs[Result].draft.get.after  mustEqual Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {\n  void inject(Object object) {}\n}")
      }
    }

    "return a deduplicate request for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(deduplicate = Some(Deduplicate(Code(name = "Name.java", description = "Name class", content = "class Name {\n\t/** {@link Name#boom(String)} **/\tvoid boom(){ System.out.println(1); }\n\tvoid baam(){ System.out.println(1); }\n\tvoid beem(){ System.out.println(1); }\n\tvoid buum(){ baam(); }\n}"))))) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[Result].draft.get.before === Code(name = "Name.java", description = "Name class", content = "class Name {\n\t/** {@link Name#boom(String)} **/\tvoid boom(){ System.out.println(1); }\n\tvoid baam(){ System.out.println(1); }\n\tvoid beem(){ System.out.println(1); }\n\tvoid buum(){ baam(); }\n}")
        responseAs[Result].draft.get.after  === Code(name = "Name.java", description = "Name class", content = "class Name {\n\t/** {@link Name#boom(String)} **/\tvoid boom(){ System.out.println(1); }\n\tvoid buum(){ boom(); }\n}")
      }
    }

    "return a deduplicate request in JSON form for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", HttpEntity(MediaTypes.`application/json`, """{"deduplicate": { "source": {"name": "Name.java", "description":"Name class", "content":"class Name {\n\t/** {@link Name#boom(String)} **/\tvoid boom(){ System.out.println(1); }\n\tvoid baam(){ System.out.println(1); }\n\tvoid beem(){ System.out.println(1); }\n\tvoid buum(){ baam(); }\n}", "tags":[], "datastructures": [], "algorithms": [], "refactorings": [], "confidence": 2, "comments":[]} }}""" )) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[Result].draft.get.before mustEqual Code(name = "Name.java", description = "Name class", content = "class Name {\n\t/** {@link Name#boom(String)} **/\tvoid boom(){ System.out.println(1); }\n\tvoid baam(){ System.out.println(1); }\n\tvoid beem(){ System.out.println(1); }\n\tvoid buum(){ baam(); }\n}")
        responseAs[Result].draft.get.after  mustEqual Code(name = "Name.java", description = "Name class", content = "class Name {\n\t/** {@link Name#boom(String)} **/\tvoid boom(){ System.out.println(1); }\n\tvoid buum(){ boom(); }\n}")
      }
    }

    "return a deduplicate request in JSON form for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", HttpEntity(MediaTypes.`application/json`, """{"deduplicate":{"source":{"name":"ScratchedCodeSnippet.java","description":"Java: *scratched* code snippet","content":"import java.util.*;\nimport java.lang.*;\n\nclass ScratchedCodeSnippet {\n  public static void main(String[] args) {\n    int[] arr = {12, 23, 43, 34, 3, 6, 7, 1, 9, 6};\n    {\n      int temp;\n      for (int i = 0; i < arr.length; i++) {\n        for (int j = 0; j < arr.length - i; j++) {\n          if (arr[j] > arr[j + 1]) {\n            temp = arr[j];\n            arr[j + 1] = arr[j];\n            arr[j + 1] = temp;\n          }\n        }\n      }\n    }\n    for (int i = 0; i < arr.length; i++) {\n      System.out.print(arr[i] + \" \");\n    }\n  }\n}\n","elapsedtime":"","tags":[],"datastructures":[],"algorithms":[],"refactorings":[],"confidence":0,"comments":[],"url":"http://stackoverflow.com/questions/12339939/bubble-sort-in-java","birthday":1406125099529}}}""" )) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[Result].info.get mustEqual Info(List("everything looks clear!"))
      }
    }

    "return a cleanup request for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(cleanup = Some(Cleanup(Code(name = "Name.java", description = "Name class", content = "class Name {\n\t/** {@link Name#boom(String)} **/\tvoid boom(){ System.out.println(1); }\n\tvoid baam(){ System.out.println(1); }\n\tvoid beem(){ System.out.println(1); }\n\tvoid buum(){ baam(); }\n}"))))) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[Result].draft.get.before === Code(name = "Name.java", description = "Name class", content = "class Name {\n  /** {@link Name#boom(String)} **/\n  void boom() {\n    System.out.println(1);\n  }\n\n  void baam() {\n    System.out.println(1);\n  }\n\n  void beem() {\n    System.out.println(1);\n  }\n\n  void buum() {\n    baam();\n  }\n}")
        responseAs[Result].draft.get.after  === Code(name = "Name.java", description = "Name class", content = "class Name {\n  /** {@link Name#boom(String)} **/\n  void boom() {\n    System.out.println(1);\n  }\n\n  void buum() {\n    boom();\n  }\n}")
      }
    }

    "return a cleanup request in JSON form for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", HttpEntity(MediaTypes.`application/json`, """{"cleanup": { "source": {"name": "Name.java", "description":"Name class", "content":"class Name {\n\t/** {@link Name#boom(String)} **/\tvoid boom(){ System.out.println(1); }\n\tvoid baam(){ System.out.println(1); }\n\tvoid beem(){ System.out.println(1); }\n\tvoid buum(){ baam(); }\n}", "tags":[], "datastructures": [], "algorithms": [], "refactorings": [], "confidence": 2, "comments":[]} }}""" )) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[Result].draft.get.before === Code(name = "Name.java", description = "Name class", content = "class Name {\n  /** {@link Name#boom(String)} **/\n  void boom() {\n    System.out.println(1);\n  }\n\n  void baam() {\n    System.out.println(1);\n  }\n\n  void beem() {\n    System.out.println(1);\n  }\n\n  void buum() {\n    baam();\n  }\n}")
        responseAs[Result].draft.get.after  === Code(name = "Name.java", description = "Name class", content = "class Name {\n  /** {@link Name#boom(String)} **/\n  void boom() {\n    System.out.println(1);\n  }\n\n  void buum() {\n    boom();\n  }\n}")
      }
    }

    "come back with a custom unauthorized request message" in {
      Get("/kiwi/find?q=roles:curators") ~> addHeader(RawHeader("x-auth-token", "gollum")) ~> sealRoute(vesperRoutes) ~> check {
        status === StatusCodes.OK
        responseAs[Result].failure.get.message === "You are not authorized to see these resources."
      }
    }

    "come back with a 401 unauthorized request message" in {
      Put("/kiwi/eval", Command(deduplicate = Some(Deduplicate(Code(name = "Name.java", description = "Name class", content = "class Name {\n\t/** {@link Name#boom(String)} **/\tvoid boom(){ System.out.println(1); }\n\tvoid baam(){ System.out.println(1); }\n\tvoid beem(){ System.out.println(1); }\n\tvoid buum(){ baam(); }\n}"))))) ~>
        addHeader(RawHeader("x-auth-token", "mal")) ~>
        sealRoute(vesperRoutes) ~> check {
        status === StatusCodes.Unauthorized
      }
    }


    "return a clip method request (no dependencies) for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(trim = Some(Trim(source = Code(name = "ScratchedCodeSnippet.java", description = "Bubble sort implementation", content = "class ScratchedCodeSnippet {\n  public static void main(String[] args) {\n    int[] arr = {12, 23, 43, 34, 3, 6, 7, 1, 9, 6};\n    {\n      int temp;\n      for (int i = 0; i < arr.length; i++) {\n        for (int j = 0; j < arr.length - i; j++) {\n          if (arr[j] > arr[j + 1]) {\n            temp = arr[j];\n            arr[j + 1] = arr[j];\n            arr[j + 1] = temp;\n          }\n        }\n      }\n    }\n    for (int i = 0; i < arr.length; i++) {\n      System.out.print(arr[i] + \" \");\n    }\n  }\n}"), where =  List(31, 496))))) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[Result].draft.get.before mustEqual  Code(name = "ScratchedCodeSnippet.java", description = "Bubble sort implementation", content = "class ScratchedCodeSnippet {\n  public static void main(String[] args) {\n    int[] arr = {12, 23, 43, 34, 3, 6, 7, 1, 9, 6};\n    {\n      int temp;\n      for (int i = 0; i < arr.length; i++) {\n        for (int j = 0; j < arr.length - i; j++) {\n          if (arr[j] > arr[j + 1]) {\n            temp = arr[j];\n            arr[j + 1] = arr[j];\n            arr[j + 1] = temp;\n          }\n        }\n      }\n    }\n    for (int i = 0; i < arr.length; i++) {\n      System.out.print(arr[i] + \" \");\n    }\n  }\n}")
        responseAs[Result].draft.get.after  mustEqual  Code(name = "ScratchedCodeSnippet.java", description = "Bubble sort implementation", content = "class ScratchedCodeSnippet {\n  public static void main(String[] args) {\n    int[] arr = {12, 23, 43, 34, 3, 6, 7, 1, 9, 6};\n    {\n      int temp;\n      for (int i = 0; i < arr.length; i++) {\n        for (int j = 0; j < arr.length - i; j++) {\n          if (arr[j] > arr[j + 1]) {\n            temp = arr[j];\n            arr[j + 1] = arr[j];\n            arr[j + 1] = temp;\n          }\n        }\n      }\n    }\n    for (int i = 0; i < arr.length; i++) {\n      System.out.print(arr[i] + \" \");\n    }\n  }\n}")
      }
    }


    "return a clip method (with dependencies) request for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(trim = Some(Trim(source = Code(name = "Name.java", description = "Name class implementation", content = "import java.util.List; \nimport java.util.Collection; \nclass Name {\nString msg = \"Hi!\";\n\tString boom(String msg){ if(null != msg) { return boom(null);} return \"Hi!\";}\n\t/** {@link Name#boom(String)}**/String baam(String msg){  return msg; }\nString beem(String text){ return boom(text); }}"), where =  List(88, 165))))) ~>
        sealRoute(vesperRoutes) ~> check {
        responseAs[Result].draft.get.before mustEqual  Code(name = "Name.java", description = "Name class implementation", content = "import java.util.List; \nimport java.util.Collection; \nclass Name {\nString msg = \"Hi!\";\n\tString boom(String msg){ if(null != msg) { return boom(null);} return \"Hi!\";}\n\t/** {@link Name#boom(String)}**/String baam(String msg){  return msg; }\nString beem(String text){ return boom(text); }}")
        responseAs[Result].draft.get.after  mustEqual  Code(name = "Name.java", description = "Name class implementation", content = "import java.util.List;\nimport java.util.Collection;\n\nclass Name {\n  String boom(String msg) {\n    if (null != msg) {\n      return boom(null);\n    }\n    return \"Hi!\";\n  }\n}")
      }
    }

    "return a cleanup request 1 for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(cleanup = Some(Cleanup(Code(name = "ToSort.java", description = "Name class", content = "import java.util.ArrayList;\nimport java.util.Collections;\nimport java.util.List;\nimport java.util.*;\n\npublic class ToSort implements Comparable {\n\n    private Float val;\n    private String id;\n\n    public ToSort(Float val, String id) {\n      this.val = val;\n      this.id = id;\n    }\n\n    @Override\n    public int compareTo(Object o) {\n\n      ToSort f = (ToSort) o;\n\n      if (val.floatValue() > f.val.floatValue()) {\n        return 1;\n      } else if (val.floatValue() < f.val.floatValue()) {\n        return -1;\n      } else {\n        return 0;\n      }\n\n    }\n\n    @Override\n    public String toString() {\n      return this.id;\n    }\n  }"))))) ~>
        sealRoute(vesperRoutes) ~> check {

        responseAs[Result].draft.get.before === Code(name = "ToSort.java", description = "Name class", content = "import java.util.ArrayList;\nimport java.util.Collections;\nimport java.util.List;\nimport java.util.*;\n\npublic class ToSort implements Comparable {\n\n  private Float val;\n  private String id;\n\n  public ToSort(Float val, String id) {\n    this.val = val;\n    this.id = id;\n  }\n\n  @Override\n  public int compareTo(Object o) {\n\n    ToSort f = (ToSort) o;\n\n    if (val.floatValue() > f.val.floatValue()) {\n      return 1;\n    } else if (val.floatValue() < f.val.floatValue()) {\n      return -1;\n    } else {\n      return 0;\n    }\n\n  }\n\n  @Override\n  public String toString() {\n    return this.id;\n  }\n}")
        responseAs[Result].draft.get.after  === Code(name = "ToSort.java", description = "Name class", content = "import java.util.*;\n\npublic class ToSort implements Comparable {\n\n  private Float val;\n  private String id;\n\n  public ToSort(Float val, String id) {\n    this.val = val;\n    this.id = id;\n  }\n\n  @Override\n  public int compareTo(Object o) {\n\n    ToSort f = (ToSort) o;\n\n    if (val.floatValue() > f.val.floatValue()) {\n      return 1;\n    } else if (val.floatValue() < f.val.floatValue()) {\n      return -1;\n    } else {\n      return 0;\n    }\n\n  }\n\n  @Override\n  public String toString() {\n    return this.id;\n  }\n}")
      }
    }

  }
}
