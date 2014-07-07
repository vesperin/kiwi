package edu.ucsc.vesper.http.core

import spray.http._
import spray.routing._

/**
 * TODO(Huascar) describe class.
 *
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 *
 */
trait CORSDirectives {
  this: HttpService =>
  private def respondWithCORSHeaders(origin: String) =
    respondWithHeaders(
      HttpHeaders.`Access-Control-Allow-Origin`(SomeOrigins(List(origin))),
      HttpHeaders.`Access-Control-Allow-Credentials`(allow = true)
    )
  private def respondWithCORSHeadersAllOrigins =
    respondWithHeaders(
      HttpHeaders.`Access-Control-Allow-Origin`(AllOrigins),
      HttpHeaders.`Access-Control-Allow-Credentials`(allow = true)
    )

  def corsFilter(origins: List[String])(route: Route) =
    if (origins.contains("*"))
      respondWithCORSHeadersAllOrigins(route)
    else
      optionalHeaderValueByName("Origin") {
        case None =>
          route
        case Some(clientOrigin) =>
          if (origins.contains(clientOrigin))
            respondWithCORSHeaders(clientOrigin)(route)
          else {
            // Maybe, a Rejection will fit better
            complete(StatusCodes.Forbidden, "Invalid origin")
          }
      }
}
