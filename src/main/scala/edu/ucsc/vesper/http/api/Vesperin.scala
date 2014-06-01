package edu.ucsc.vesper.http.api

import spray.routing.HttpService
import edu.ucsc.vesper.http.core._
import edu.ucsc.vesper.http.domain.Models.Command
import spray.httpx.SprayJsonSupport
import SprayJsonSupport._

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
trait Vesperin extends HttpService with AsyncSupport with UserLounge {

  /**
   * The reason I decided to desist from adding thousands of instances of a some Interpreter actor
   * to perform vesper's operations and use a combination of a VesperInterpreter, detatch() directive,
   * and the onComplete Future directive is the following:
   *
   * `eval` might take some time come up with its Future result (e.g. because it needs to parse the Java code
   * in order to perform the desired refactoring). It then returns the Future because it will again take some time
   * to actually create a `ChangeSummary`.  I.e. we are potentially dealing with two separate "costly" operations
   * here:
   *
   * 1. Parsing code and then performing refactoring
   * 2. Creating a ChangeSummary object
   *
   * The first operation is "protected" with the detach() and the second one by returning a Future.
   *
   * @see <pre><code> http://www.chrisstucchio.com/blog/2013/actors_vs_futures.html</code></pre>
   *
   */
  val interpreter : VesperInterpreter = VesperInterpreter()

  val describe =
    path("help") {
      get {
        complete{
          """|Hello, guest. I am Vesper; a lightweight source code curating framework for Java.
             |My API's current version is 0.0.1.
             |I'm maintained by Huascar A. Sanchez.
             |My mission is simply to allow code foragers to cure source code on the Web.""".stripMargin
        }
      }
    }

  val vesper =
    path("vesper"){
      authenticate(vesperin) { membership =>
        post {
          authorize(isCurator(membership)){
            entity(as[Command]) {
              command =>
                detach(){
                  onComplete(interpreter.eval(membership, command)){
                    case result => complete(result)
                  }
                }
            }
          }
        } ~
        get {
          authorize(isReviewer(membership)){
            parameter('c){
              c =>
                detach(){
                  onComplete(interpreter.eval(membership, c)){
                    case result => complete(result)
                  }
                }
            }
          }
        }
      }
    }

  val vesperRoutes =
    pathPrefix("api") {
      describe ~ vesper
    }
}
