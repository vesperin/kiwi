package com.vesperin.kiwi.domain

import spray.httpx.SprayJsonSupport
import spray.json.DefaultJsonProtocol

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
case class Stage(
    label: String
    , method: String
    , isBase: Boolean
    , where: List[List[Int]] = List()
    , source: Code
    , budget: Int = 15
)

object Stage extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val stageFormats = jsonFormat6(Stage.apply)
}