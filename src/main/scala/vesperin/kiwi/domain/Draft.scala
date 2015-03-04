package vesperin.kiwi.domain

import spray.httpx.SprayJsonSupport
import spray.json.DefaultJsonProtocol

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
case class Draft(
    /** The cause of reason some code was changed **/
    cause: String

    /** The full description of why this code was changed **/
    , description: String

    /** When was this code changed **/
    , timestamp: Long

    /** The code before the change **/
    , before: Code

    /** The code after the change **/
    , after: Code
)

object Draft extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val codeFormats = jsonFormat5(Draft.apply)
}
