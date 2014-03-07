package edu.ucsc.vesper.http.api

import spray.routing.HttpService
import edu.ucsc.vesper.http.core._
import edu.ucsc.vesper.http.domain.LoungeObjects.{ChangeSummary, Answer, Command}
import akka.actor.Props
import akka.routing.RoundRobinRouter
import edu.ucsc.vesper.http.core.Get

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
trait Vesperin extends HttpService with AsyncSupport with UserLounge {
  def backend = actorRefFactory.actorOf(
    Props[InterpreterActor].withRouter(
      RoundRobinRouter(
        nrOfInstances = 1000
      )
    )
  )

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
      authenticate(vesperin) { membership =>
        get {
          authorize(isReviewer(membership)){
            parameter('q){
              q =>
                complete((backend ? Get(membership.role, q)).mapTo[Option[Answer]])
            }
          }
        }
      }
    }

  val curate =
    path("try"){
      authenticate(vesperin) { membership =>
        (put | post | parameter('method ! "c")) {
          authorize(isCurator(membership)){
            entity(as[Command]) {
              command =>
                complete((backend ? Perform(membership.auth, command)).mapTo[Option[ChangeSummary]])
            }
          }
        }
      }
    }

  val vesperRoutes =
    pathPrefix("api") {
      greetings ~ search ~ curate
    }
}
