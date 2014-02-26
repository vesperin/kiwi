package edu.ucsc.vesper.domain

import spray.json.DefaultJsonProtocol
import spray.httpx.SprayJsonSupport

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
case class Command (name: String, params: List[String])

object Command extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val commandFormats = jsonFormat2(Command.apply)
}
