package edu.ucsc.vesper.http.domain

import spray.httpx.SprayJsonSupport
import spray.json.DefaultJsonProtocol

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
case class Body(kind: String, id: String, longUrl: String)

object Body extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val bodyFormats = jsonFormat3(Body.apply)
}
