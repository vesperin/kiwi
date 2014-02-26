package edu.ucsc.vesper.domain

import java.util.Date

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
case class Curating (
      id:           Option[Long],           // the comment unique id
      sessionname:  String,                 // the name of the session
      username:     String,                 // the username of the user who started a curating session
      password:     String,                 // the password of the user who started a curating session
      gistid:       Option[Long] = None,    // the gist id corresponding to the Source being curated
      url:          String,                 // the url where this Source was found
      birthday:     Option[Date]            // when this Source was first curated.
)
