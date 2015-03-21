package vesperin.kiwi.domain

import spray.httpx.SprayJsonSupport
import spray.json.DefaultJsonProtocol

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
// Find(any: Option[], all: Option[] ...
case class AnyInSet(name: String, targets: List[String])

case class ExactlyOne(name: String, value: String)

case class ExactlyAllInSet(name: String, targets: List[String])

case class AllInSet(symbol: String = "*")

case class ExactRole(value: String)

case class ById(value: String)

case class Find(
      all: Option[AllInSet]               = None,
      any: Option[AnyInSet]               = None,
      exact: Option[ExactlyOne]           = None,
      exactlyAll:Option[ExactlyAllInSet]  = None,
      byId: Option[ById]                  = None,
      roles: Option[ExactRole]            = None
)

object AllInSet extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val allFormat = jsonFormat1(AllInSet.apply)
}

object AnyInSet extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val anyFormat = jsonFormat2(AnyInSet.apply)
}

object ExactlyOne extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val exactFormat = jsonFormat2(ExactlyOne.apply)
}

object ExactRole extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val exactRoleFormat = jsonFormat1(ExactRole.apply)
}

object ExactlyAllInSet extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val exactlyAllFormat = jsonFormat2(ExactlyAllInSet.apply)
}

object ById extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val byIdFormat = jsonFormat1(ById.apply)
}

object Find extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val findFormats = jsonFormat6(Find.apply)
}
