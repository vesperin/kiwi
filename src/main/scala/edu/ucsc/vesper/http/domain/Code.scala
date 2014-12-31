package edu.ucsc.vesper.http.domain

import sprest.models.{Model, ModelCompanion}

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
case class Code (
    /** the name of the source code (e.g., filename) **/
    name: String

    /** a piece of text describing this source code; i.e., the synopsis **/
    , description: String

    /** the current content of this Source **/
    , content: String

    /** the curation elapsed time **/
    , elapsedtime: Option[String]   = None

    /**
     * tags or labels categorizing this source code. This
     * information gives the context of source code; e.g., Cracking code interview.
     */
    , tags: List[String]            = List()

    /** a sub-category indicating the data structures used in this source code **/
    , datastructures: List[String]  = List()

    /** a sub-category indicating the algorithms used is this source code **/
    , algorithms: List[String]      = List()

    /**
     * The unique refactorings that were used that led to this transformed source code.
     * This may give us some insight of what was on the programmer's mind when changing
     * this source code.
     */
    , refactorings: List[String]    = List()

    /**
     * Level of confidence with respect to the quality of the final result (source code); e.g., stars?
     */
    , confidence: Int               = 0

    /** the url where this Source was found **/
    , url: Option[String]           = None

    /** The date when this source code was found **/
    , birthday: Option[Long]        = None

    /** comments describing this source code. **/
    , comments: List[Comment]       = List()

    /** the code unique id **/
    , var id: Option[String]   = None
) extends Model[String]

object Code extends ModelCompanion[Code, String] {
  implicit val codeFormats = jsonFormat13(Code.apply)
}