package com.vesperin.kiwi.api

import com.vesperin.kiwi.auth.AuthSupport
import com.vesperin.kiwi.domain.Command
import com.vesperin.kiwi.interpret.CommandInterpreter
import spray.httpx.SprayJsonSupport._
import spray.routing.HttpService

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
trait Kiwi extends HttpService with AsyncSupport with AuthSupport {

  /**
   * The reason I decided to desist from adding thousands of instances of some Interpreter actor
   * to perform Kiwi's operations and use a combination of a CommandInterpreter, detatch() directive,
   * and the onComplete Future directive is the following:
   *
   * `eval` might take some time come up with its Future result (e.g. because it needs to parse the Java code
   * in order to perform the desired refactoring). It then returns the Future because it will again take some time
   * to actually create a `Result`.  I.e. we are potentially dealing with two separate "costly" operations
   * here:
   *
   * 1. Parsing code and then performing some operation
   * 2. Creating a Result object
   *
   * The first operation is "protected" with the detach() and the second one by returning a Future.
   *
   * @see <pre><code> http://www.chrisstucchio.com/blog/2013/actors_vs_futures.html</code></pre>
   *
   */
  val interpreter : CommandInterpreter = CommandInterpreter()

  val help =
    path("help") {
      get {
        getFromResource("html/index.html")
      }
    }

  val eval =
    path("eval"){
      authenticate(membership) { membership =>
        (post | put) {
          authorize(isCurator(membership)){
            entity(as[Command]) {
              command =>
                // http://spray.io/documentation/1.2.2/spray-routing/execution-directives/detach/#detach
                // This directive needs either an implicit ExecutionContext (detach()) or an explicit one (detach(ec)).
                detach(executionContext){
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
      authenticate(membership) { membership =>
        get {
          authorize(isReviewer(membership)){
            parameter('q){
              q =>
                detach(executionContext){
                  onComplete(interpreter.eval(membership, q)){
                    case result => complete(result)
                  }
                }
            }
          }
        }
      }
    }

  val index = path("") {
    get {
      detach(executionContext){
        onComplete(interpreter.statusPage()){
          case result => complete(result)
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
            detach(executionContext){
              onComplete(interpreter.render(q, s)){
                case result => complete(result)
              }
            }
        }
      }
    }

  val api =
    pathPrefix("kiwi") {
      help ~ eval ~ find ~ render
    }

  val routes = index ~ api
}
