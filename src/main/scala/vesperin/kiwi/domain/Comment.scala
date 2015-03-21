package vesperin.kiwi.domain

import spray.httpx.SprayJsonSupport
import spray.json.DefaultJsonProtocol

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
case class Comment (
   /** location's lower bound; e.g., "line;col;offset" **/
   from: String

   /** location's upper bound; e.g., "line;col;offset" **/
   , to: String

   /** the comment's text **/
   , text: String

   /** the comment's author **/
   , username: Option[String]  = Some("You")
)

object Comment extends DefaultJsonProtocol
  with SprayJsonSupport {
  implicit val commentFormats = jsonFormat4(Comment.apply)
}