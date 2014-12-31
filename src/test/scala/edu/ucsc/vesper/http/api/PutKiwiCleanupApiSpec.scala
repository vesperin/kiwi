package edu.ucsc.vesper.http.api

import edu.ucsc.vesper.http.domain.{Code, Deduplicate, Command}
import org.specs2.mutable.Specification
import spray.http.HttpHeaders.RawHeader
import spray.http.StatusCodes
import spray.testkit.Specs2RouteTest

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
class PutKiwiCleanupApiSpec extends Specification with Specs2RouteTest with Kiwi {
  def actorRefFactory = system

  "Kiwi" should {
    "Return a 401 unauthorized request message after a deduplication POST request to the root path" in {
      Put("/kiwi/eval", Command(deduplicate = Some(Deduplicate(Code(name = "Name.java", description = "Name class", content = "class Name {\n\t/** {@link Name#boom(String)} **/\tvoid boom(){ System.out.println(1); }\n\tvoid baam(){ System.out.println(1); }\n\tvoid beem(){ System.out.println(1); }\n\tvoid buum(){ baam(); }\n}"))))) ~>
        addHeader(RawHeader("x-auth-token", "mal")) ~>
        sealRoute(routes) ~> check {
        status === StatusCodes.Unauthorized
      }
    }
  }
}
