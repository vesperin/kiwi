package edu.ucsc.vesper.http.api

import spray.routing.HttpService
import edu.ucsc.vesper.http.domain.LoungeObjects._
import edu.ucsc.vesper.http.core.UserLounge

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
trait Vesperin extends HttpService with UserLounge {
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
                entity(as[Request]) {
                  request =>
                    complete(request)
                }
              }
            }
        }
      }
    }
}
