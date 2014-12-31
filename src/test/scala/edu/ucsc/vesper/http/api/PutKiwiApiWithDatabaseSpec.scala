package edu.ucsc.vesper.http.api

import java.util.Date

import edu.ucsc.vesper.http.domain._
import org.specs2.mutable.Specification
import spray.testkit.Specs2RouteTest

import scala.concurrent.duration._

/**
 * This class covers any DB tests for vesper; e.g., storing code snippet, finding code snippet, rendering
 * a nice HTML pages for a code snippet, etc.
 *
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 *
 */
class PutKiwiApiWithDatabaseSpec extends Specification with Specs2RouteTest with Kiwi {
  def actorRefFactory = system

  // reason of this addition? see https://groups.google.com/forum/#!msg/spray-user/o8DtI6VUMbA/n9tguTb_1noJ
  implicit val routeTestTimeout = RouteTestTimeout(FiniteDuration(5, SECONDS))

  def cleanupDatabase = {
    interpreter.storage.clear()
  }

  "Kiwi" should {
    "return Code snippet was saved! on a put request and perform a database insert " in {
      Put("/kiwi/eval?auth_token=legolas", Command(persist = Some(Persist(
        Code(
          name = "GCD.java",
          description     = "Implements the GCD algorithm cc @codedetour.",
          content         = "class GCD {\t\t\n\t\tstatic int computeGcd(int a, int b){\n\t\t return BigInteger.valueOf(a).gcd(BigInteger.valueOf(b)).intValue(); \n\t\t}\n}",
          elapsedtime     = Some("00:05:00"),
          tags            = List("Slow", "Inefficient", "WhyThis"),
          datastructures  = List("BigInteger"),
          algorithms      = List("GCD"),
          refactorings    = List("Full clean up"),
          confidence      = 2,
          url             = Some("http://www.programmingtask.com"),
          birthday        = Some(new Date().getTime),
          comments        = List(Comment(from = "1;1;0", to = "2;10;234", text = "BigInteger's gcd seems very expensive"))))))) ~>
        sealRoute(routes) ~> check {
        responseAs[Result] mustEqual Result(info = Some(Info(List("GCD.java was saved, then tweeted by @codetour"))))
      }
    }
  }

}
