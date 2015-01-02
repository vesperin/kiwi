package com.vesperin.kiwi.domain

import spray.httpx.SprayJsonSupport
import spray.json.DefaultJsonProtocol

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
case class LongURL(longUrl: String)

object LongURL extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val urlFormats = jsonFormat1(LongURL.apply)
}