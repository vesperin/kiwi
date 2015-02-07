package com.vesperin.kiwi.domain

import spray.httpx.SprayJsonSupport
import spray.json.DefaultJsonProtocol

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
// the command to request some result
case class Command(
      inspect:      Option[Inspect]     = None,
      remove:       Option[Remove]      = None,
      rename:       Option[Rename]      = None,
      optimize:     Option[Optimize]    = None,
      format:       Option[Format]      = None,
      deduplicate:  Option[Deduplicate] = None,
      cleanup:      Option[Cleanup]     = None,
      publish:      Option[Publish]     = None,
      persist:      Option[Persist]     = None,
      preprocess:   Option[Preprocess]  = None,
      find:         Option[Find]        = None,
      slice:        Option[Slice]       = None,
      multistage:   Option[Multistage]  = None,
      summarize:    Option[Summarize]   = None
)

object Command extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val requestFormats = jsonFormat14(Command.apply)
}
