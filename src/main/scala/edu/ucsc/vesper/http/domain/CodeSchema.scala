package edu.ucsc.vesper.http.domain

import sprest.models.{Model, ModelCompanion}
import org.joda.time.DateTime
import spray.json.DefaultJsonProtocol
import spray.httpx.SprayJsonSupport

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
case class CommentSchema (
   /** location's lower bound; e.g., "line;col;offset" **/
   from: String

   /** location's upper bound; e.g., "line;col;offset" **/
   , to: String

   /** the comment's text **/
   , text: String
)

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
case class CheckpointSchema(
   /** the Source content **/
   content: String

   /** comments describing this source code. **/
   , comments: List[CommentSchema]
)

/**
 *
 * The Code schema. This schema is immutable at the persistence level.
 * This mean that once stored no updates or overrides will be made. The only
 * permitted operation, besides querying, is deletion. What if the user
 * wants change his/her mind and want to perform different edits on an already
 * stored snippet? The answer is that he/she could definitely start from an
 * already curated code, however, when he/she finishes curating the new version,
 * this version will become a brand new code snippet.
 *
 * From the above, it is ok for entries in a collection to have same names and probably
 * same descriptions. However, their id will be unique and their contents similar; i.e.,
 * they will be clones.
 *
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
case class CodeSchema(
    /** the name of the source code (e.g., filename) **/
    name:         String

    /** the Source brief description **/
    , description:  String

    /** the latest Source content **/
    , content:      String

    /** the Source's tags **/
    , tags:         List[String]

    /** the url where this Source was found **/
    , url:          String

    /** when this Source was first curated. **/
    , birthday:     Option[DateTime]

    /** comments describing this source code. **/
    , comments:     List[CommentSchema]

    /** history of revisions. **/
    , history:      List[CheckpointSchema]  // the last will be the original `found` code

    /** the code unique id **/
    , var id: Option[String] = None

) extends Model[String]


object CommentSchema extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val commentFormats = jsonFormat3(CommentSchema.apply)
}

object CheckpointSchema extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val checkpointFormats = jsonFormat2(CheckpointSchema.apply)
}

object CodeSchema extends ModelCompanion[CodeSchema, String]{
  import sprest.Formats._
  implicit val codeSnippetJsonFormat = jsonFormat9(CodeSchema.apply _)
}
