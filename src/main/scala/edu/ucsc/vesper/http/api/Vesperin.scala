package edu.ucsc.vesper.http.api

import edu.ucsc.vesper.http.core._
import edu.ucsc.vesper.http.domain.Models.Command
import spray.httpx.SprayJsonSupport._
import spray.routing.HttpService

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
        complete {
          interpreter.renderHelpPage()
        }
      }
    }

  val eval =
    path("eval"){
      authenticate(vesperin) { membership =>
        (post | put) {
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
        }
      }
    }

  val find =
    path("find") {
      authenticate(vesperin) { membership =>
        get {
          authorize(isReviewer(membership)){
            parameter('q){
              q =>
                detach(){
                  onComplete(interpreter.eval(membership, q)){
                    case result => complete(result)
                  }
                }
            }
          }
        }
      }
    }

  val render =
    path("render") {
      get {
        parameters('q, 's ? "on"){
          // note (Huascar): IntelliJ is detecting an error where there is none
          // this is how to use parameters in Spray.io
          (q, s) =>
            detach(){
              onComplete(interpreter.render(q, s)){
                case result => complete(result)
              }
            }
        }
      }
    }

  val vesperRoutes =
    pathPrefix("kiwi") {
      describe ~ eval ~ find ~ render
    }
}
