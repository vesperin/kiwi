package edu.ucsc.vesper.http.api

import spray.routing.HttpService
import edu.ucsc.vesper.http.core.{Get, InterpreterActor, AsyncSupport, UserLounge}
import edu.ucsc.vesper.http.domain.LoungeObjects.{Answer, Command}
import akka.actor.Props
import akka.routing.RoundRobinRouter

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
            "Morning, guest."
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
                      complete(request)
                  }
                }
              }
          }
        }
    }
}
