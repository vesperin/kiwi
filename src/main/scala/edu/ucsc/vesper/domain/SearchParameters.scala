package edu.ucsc.vesper.domain

import java.util.Date

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
case class SearchParameters(
     id: Option[Long]                  = None,
     sessionname:Option[String]        = None,
     username: Option[String]          = None,
     password: Option[String]          = None,
     gistid: Option[Long]              = None,
     birthday: Option[Date]            = None
)
