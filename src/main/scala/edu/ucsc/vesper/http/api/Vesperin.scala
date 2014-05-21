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
   * 1. Parsing code and then performing refactoring
   * 2. Creating a ChangeSummary object
   *
   * The first operation is "protected" with the detach() and the second one by returning a Future.
   *
   */
  val interpreter : VesperInterpreter = VesperInterpreter()
  val storage : VesperStorage         = VesperStorage()

  val greetings =
    path("hi") {
      get {
        complete{
          "Hello, guest. I am Vesper; a lightweight source code curating framework for Java"
        }
      }
    }

  val search =
    path("search") {
      // TODO (Huascar): Improve the search mechanism so that Vesper can take queries (simple and complex ones)
      // TODO (Huascar): Once this update is done, please consider removing the `emit` directive
      authenticate(vesperin) { membership =>
        get {
          authorize(isReviewer(membership)){
            parameter('q){
              q =>
                detach(){
                  onComplete(interpreter.ask(membership.role, q)){
                    case answer => complete(answer)
                  }
                }
            }
          }
        }
      }
    }

  val cure =
    path("cure"){
      authenticate(vesperin) { membership =>
        (post | parameter('method ! "c")) {
          authorize(isCurator(membership)){
            entity(as[Command]) {
              command =>
                detach(){
                  onComplete(interpreter.eval(membership.auth, command)){
                    case changeSummary => complete(changeSummary)
                  }
                }
            }
          }
        }
      }
    }

  val persist =
      path("persist"){
        authenticate(vesperin) { membership =>
          /** Put the <new_codesnippet> on the server! **/
          put {
            authorize(isCurator(membership)){
              entity(as[Command]) {
                command =>
                  detach(){
                    onComplete(storage.persist(command)){
                      case changeSummary => complete(changeSummary)
                    }
                  }
              }
            }
          }
        }
      }

  val emit =
    path("snippets"){
      authenticate(vesperin) { membership =>
        get {
          authorize(isReviewer(membership)){
            detach() {
              onComplete(storage.findAll()) {
                case codes => complete(codes)
              }
            }
          }
        }
      }
    }

  val vesperRoutes =
    pathPrefix("api") {
      greetings ~ search ~ cure ~ persist ~ emit
    }
}
