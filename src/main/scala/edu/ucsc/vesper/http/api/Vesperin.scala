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

  val vesperRoutes =
    pathPrefix("api") {
      path("all") {
        get {
          complete{
            "Hello, guest. I am Vesper; a lightweight source code curating framework for Java"
          }
        }
      } ~
        path("try"){
          authenticate(vesperin) { membership =>
            get {
              authorize(isReviewer(membership)){
                parameter('q){
                  q =>
                    complete((backend ? Get(membership.role, q)).mapTo[Option[Answer]])
                }
              }
            } ~
              (put | post | parameter('method ! "c")) {
                authorize(isCurator(membership)){
                  entity(as[Command]) {
                    request =>
                      complete((backend ? Curate(membership.auth, request)).mapTo[Option[ChangeSummary]])
                  }
                }
              }
          }
        }
    }
}
