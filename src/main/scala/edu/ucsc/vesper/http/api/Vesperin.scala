package edu.ucsc.vesper.http.api

import spray.routing.HttpService
import edu.ucsc.vesper.http.core.{AsyncSupport, UserLounge}
import edu.ucsc.vesper.http.domain.LoungeObjects.Command

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
trait Vesperin extends HttpService with AsyncSupport with UserLounge {
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
                    complete {
                      "Morning,  " + membership.auth.userId + ". You asked: " + s"'$q'"
                    }
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
