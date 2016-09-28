package com.vesperin.kiwi.domain

import spray.httpx.SprayJsonSupport
import spray.json.DefaultJsonProtocol

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
case class Block(
     section: String,
     from:List[Int]/*[line, offset]*/,
     to: List[Int]/*[line, offset]*/
)

object Block extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val bodyFormats = jsonFormat3(Block.apply)
}
