package vesperin.kiwi.domain

import spray.httpx.SprayJsonSupport
import spray.json.DefaultJsonProtocol

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
sealed trait Intention

case class Inspect(
     source: Code,
     imports:Boolean = false
) extends Intention

case class Remove(
     what: String,
     where: List[Int],
     source: Code,
     preprocess:Boolean = false
) extends Intention

case class Rename(
     what: String,
     where: List[Int],
     to: String,
     source: Code,
     preprocess:Boolean = false
) extends Intention

case class Optimize(
     source: Code
) extends Intention

case class Format(
     source: Code
) extends Intention

case class Deduplicate(
     source: Code,
     preprocess:Boolean = false
) extends Intention

case class Cleanup(
     source: Code,
     preprocess:Boolean = false
) extends Intention

case class Publish(
     drafts: List[Draft]
) extends Intention

case class Persist(
     source: Code
) extends Intention

case class Update(
     source: Code
) extends Intention

case class Pack(
     source: Code,
     preprocess:Boolean = false,
     imports:Boolean = false
) extends Intention

case class Preprocess(
     source: Code
) extends Intention

case class Slice(
     source: Code,
     where: List[Int],
     preprocess:Boolean = false
) extends Intention

case class Multistage(
     source: Code,
     budget: Int = 15,
     preprocess:Boolean = false
) extends Intention

case class Summarize(
     stage: Stage,
     preprocess:Boolean = false
) extends Intention


object Inspect extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val inspectFormats = jsonFormat2(Inspect.apply)
}

object Remove extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val deleteFormats = jsonFormat4(Remove.apply)
}

object Rename extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val renameFormats = jsonFormat5(Rename.apply)
}

object Optimize extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val optimizeFormats = jsonFormat1(Optimize.apply)
}

object Format extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val formatFormats = jsonFormat1(Format.apply)
}

object Deduplicate extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val deduplicateFormats = jsonFormat2(Deduplicate.apply)
}

object Cleanup extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val cleanupFormats = jsonFormat2(Cleanup.apply)
}

object Publish extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val publishFormats = jsonFormat1(Publish.apply)
}

object Persist extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val persistFormats = jsonFormat1(Persist.apply)
}

object Update extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val updateFormats = jsonFormat1(Update.apply)
}

object Pack extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val packFormats = jsonFormat3(Pack.apply)
}

object Preprocess extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val preprocessFormats = jsonFormat1(Preprocess.apply)
}

object Slice extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val clipFormats = jsonFormat3(Slice.apply)
}

object Multistage extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val multiStageFormat = jsonFormat3(Multistage.apply)
}

object Summarize extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val summarizeFormat = jsonFormat2(Summarize.apply)
}
