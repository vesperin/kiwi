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
          <html style="font-size: 62.5%;-webkit-font-smoothing: antialiased;font-smoothing: antialiased;">
                <body style="min-height: 100%;font-family: Courier, monospace;font-size: 1.6rem;padding: 0 2rem;color: rgb(20%,20%,20%);background: rgb(97%,97%,97%);">
                    <h1 style="font-size: 1.6rem;margin:1rem 0; line-height: 0.9em;letter-spacing: 0.02em;text-shadow: 0px 0px 6px rgba(0,0,0,0.2);">Hello, Guest!</h1>
                    <div>
                        <p>I am Vesper; a lightweight source code curating framework for Java.</p>
                        <p>I am still at alpha state. This means I may have some bugs that need to be fixed.
                           However, you can still use me for your curating activities.
                        </p>
                        <p>You can report any experienced bugs to my maintainer: <a href="&#x6d;&#97;&#x69;&#108;&#116;&#x6f;&#58;&#104;&#115;&#x61;&#110;&#x63;&#x68;&#x65;&#x7a;&#x40;&#x63;&#115;&#x2e;&#x75;&#x63;&#x73;&#99;&#46;&#x65;&#100;&#117;">Huascar Sanchez</a></p>
                      <p>Cheers,<br/>The Vesper</p>
                    </div>
                </body>
          </html>
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
                    case result => complete {
                      if(q.contains("id:")){  // HACK: can we do better than this?
                        interpreter.renderAsHtml(result)
                      } else {
                        result
                      }
                    }
                  }
                }
            }
          }
        }
      }
    }

  val vesperRoutes =
    pathPrefix("vesper") {
      describe ~ eval ~ find
    }
}
