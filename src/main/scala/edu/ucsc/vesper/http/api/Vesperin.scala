package edu.ucsc.vesper.http.api

import spray.routing.HttpService
import edu.ucsc.vesper.http.domain.LoungeObjects._
import edu.ucsc.vesper.http.core.{InterpreterActor, AsyncSupport, UserLounge}
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
        authenticate(membersOnly) { membership =>
          get {
            authorize(inTheClub(membership)){
              parameter('q){
                q =>
                  complete {
                    "Morning,  " + membership.auth.userId + ". You asked: " + s"'$q'"
                  }
              }
            }
          } ~
            (put | post | parameter('method ! "c")) {
              authorize(inTheClub(membership)){
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
