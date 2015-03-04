package vesperin.kiwi

import java.util.Date

import org.specs2.mutable.Specification
import spray.testkit.Specs2RouteTest
import vesperin.kiwi.domain._
import vesperin.kiwi.routes.Kiwi

import scala.concurrent.duration._

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
class PostKiwiApiWithDatabaseSpec extends Specification with Specs2RouteTest with Kiwi {
  def actorRefFactory = system

  // reason of this addition? see https://groups.google.com/forum/#!msg/spray-user/o8DtI6VUMbA/n9tguTb_1noJ
  implicit val routeTestTimeout = RouteTestTimeout(FiniteDuration(5, SECONDS))

  "Kiwi" should {
    "return Code snippet was updated! on a post request and perform a database update " in {
      Post("/kiwi/eval?auth_token=legolas", Command(update = Some(Update(
        Code(
          id = "f57c9c1e-d33f-4985-8ba8-eb8998c18741",
          name = "GCD.java",
          description     = "Implements an inefficient GCD algorithm",
          content         = "class GCD {\t\t\n\t\tstatic int computeGcd(int a, int b){\n\t\t return BigInteger.valueOf(a).gcd(BigInteger.valueOf(b)).intValue(); \n\t\t}\n}",
          elapsedtime     = Some("00:05:00"),
          tags            = List("Slow", "Inefficient", "BAD", "DONTUSE"),
          datastructures  = List("BigInteger"),
          algorithms      = List("GCD"),
          refactorings    = List("Full clean up"),
          confidence      = 2,
          url             = Some("http://www.programmingtask.com"),
          birthday        = Some(new Date().getTime),
          comments        = List(Comment(from = "1;1;0", to = "2;10;234", text = "BigInteger's gcd seems very expensive"))))))) ~>
        sealRoute(routes) ~> check {
        responseAs[Result].info.get.messages contains "GCD.java was updated" //.contains("GCD.java was saved, then tweeted by @codetour")
      }
    }
  }

}
