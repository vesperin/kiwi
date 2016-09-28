package com.vesperin.kiwi.api

import com.vesperin.kiwi.domain._
import org.specs2.mutable.Specification
import spray.testkit.Specs2RouteTest

import scala.concurrent.duration._

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
class PostKiwiInspectApiSpec extends Specification with Specs2RouteTest with Kiwi {
  def actorRefFactory = system

  implicit val routeTestTimeout = RouteTestTimeout(FiniteDuration(5, SECONDS))

  "Kiwi" should {
    "Return an inspect request for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(inspect = Some(Inspect(Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(Object object){}}"))))) ~>
        sealRoute(routes) ~> check {

        responseAs[Result] mustEqual Result(warnings = Some(List()))
      }
    }

    "Return an inspect request containing a warning for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(inspect = Some(Inspect(Code(name = "Random.java", description = "Lalalalal", content = "import java.util.PriorityQueue;\nimport java.util.List;\nimport java.util.ArrayList;\nimport java.util.Collections;\n\nclass Vertex implements Comparable<Vertex>\n{\n    public final String name;\n    public Edge[] adjacencies;\n    public double minDistance = Double.POSITIVE_INFINITY;\n    public Vertex previous;\n    public Vertex(String argName) { name = argName; }\n    public String toString() { return name; }\n    public int compareTo(Vertex other)\n    {\n        return Double.compare(minDistance, other.minDistance);\n    }\n\n}\n\n\nclass Edge\n{\n    public final Vertex target;\n    public final double weight;\n    public Edge(Vertex argTarget, double argWeight)\n    { target = argTarget; weight = argWeight; }\n}\n\npublic class Dijkstra\n{\n    public static void computePaths(Vertex source)\n    {\n        source.minDistance = 0.;\n        PriorityQueue<Vertex> vertexQueue = new PriorityQueue<Vertex>();\n    vertexQueue.add(source);\n\n    while (!vertexQueue.isEmpty()) {\n        Vertex u = vertexQueue.poll();\n\n            // Visit each edge exiting u\n            for (Edge e : u.adjacencies)\n            {\n                Vertex v = e.target;\n                double weight = e.weight;\n                double distanceThroughU = u.minDistance + weight;\n        if (distanceThroughU < v.minDistance) {\n            vertexQueue.remove(v);\n\n            v.minDistance = distanceThroughU ;\n            v.previous = u;\n            vertexQueue.add(v);\n        }\n            }\n        }\n    }\n\n    public static List<Vertex> getShortestPathTo(Vertex target)\n    {\n        List<Vertex> path = new ArrayList<Vertex>();\n        for (Vertex vertex = target; vertex != null; vertex = vertex.previous)\n            path.add(vertex);\n\n        Collections.reverse(path);\n        return path;\n    }\n\n    public static void main(String[] args)\n    {\n        // mark all the vertices \n        Vertex A = new Vertex(\"A\");\n        Vertex B = new Vertex(\"B\");\n        Vertex D = new Vertex(\"D\");\n        Vertex F = new Vertex(\"F\");\n        Vertex K = new Vertex(\"K\");\n        Vertex J = new Vertex(\"J\");\n        Vertex M = new Vertex(\"M\");\n        Vertex O = new Vertex(\"O\");\n        Vertex P = new Vertex(\"P\");\n        Vertex R = new Vertex(\"R\");\n        Vertex Z = new Vertex(\"Z\");\n\n        // set the edges and weight\n        A.adjacencies = new Edge[]{ new Edge(M, 8) };\n        B.adjacencies = new Edge[]{ new Edge(D, 11) };\n        D.adjacencies = new Edge[]{ new Edge(B, 11) };\n        F.adjacencies = new Edge[]{ new Edge(K, 23) };\n        K.adjacencies = new Edge[]{ new Edge(O, 40) };\n        J.adjacencies = new Edge[]{ new Edge(K, 25) };\n        M.adjacencies = new Edge[]{ new Edge(R, 8) };\n        O.adjacencies = new Edge[]{ new Edge(K, 40) };\n        P.adjacencies = new Edge[]{ new Edge(Z, 18) };\n        R.adjacencies = new Edge[]{ new Edge(P, 15) };\n        Z.adjacencies = new Edge[]{ new Edge(P, 18) };\n\n\n        computePaths(A); // run Dijkstra\n        System.out.println(\"Distance to \" + Z + \": \" + Z.minDistance);\n        List<Vertex> path = getShortestPathTo(Z);\n        System.out.println(\"Path: \" + path);\n    }\n}"))))) ~> sealRoute(routes) ~> check {
        responseAs[Result] mustEqual Result(warnings = Some(List(Warning("The public type Dijkstra must be defined in its own file. Location(line=30, start=718, end=725)."))))
      }
    }

    "Return an inspect request containing a list of imports for POST requests to the root path" in {
      Post("/kiwi/eval?auth_token=legolas", Command(inspect = Some(Inspect(source = Code(name = "Bootstrap.java", description = "Resource Injector", content = "class Bootstrap {void inject(List<Object> object){}}"), imports = true)))) ~>
        sealRoute(routes) ~> check {

        responseAs[Result] mustEqual Result(info = Some(Info(List("java.util.List;"))))
      }
    }
  }
}
