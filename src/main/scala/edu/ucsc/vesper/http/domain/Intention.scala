package edu.ucsc.vesper.http.domain

import spray.httpx.SprayJsonSupport
import spray.json.DefaultJsonProtocol

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
sealed trait Intention
// e.g., """{ "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"class Bootstrap {void inject(Object object){}"} }"""
case class Inspect(source: Code, imports:Boolean = false) extends Intention
// """{ "what": "class", "where": ["123", "132"], "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"class Bootstrap {void inject(Object object){}"} }"""
case class Remove(what: String, where: List[Int], source: Code) extends Intention
//"""{"rename" : { "what": "class", "where": [1, 2], "to":"ResourceInjector", "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"class Bootstrap {void inject(Object object){}"} }}"""
case class Rename(what: String, where: List[Int], to: String, source: Code) extends Intention
// """{ "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"import java.util.List; \n public class Bootstrap {void inject(Object object){}"} }"""
case class Optimize(source: Code) extends Intention
// """{ "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"public class Bootstrap {void inject(Object object){}"} }"""
case class Format(source: Code) extends Intention
// """{ "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"public class Bootstrap {void inject(Object object){}"} }"""
case class Deduplicate(source: Code) extends Intention
// """{ "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"public class Bootstrap {void inject(Object object){}"} }"""
case class Cleanup(source: Code) extends Intention
// drafts' sources
case class Publish(drafts: List[Draft]) extends Intention
// """{ "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"public class Bootstrap {void inject(Object object){}"} }"""
case class Persist(source: Code) extends Intention
// """{"source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"class Bootstrap {void inject(Object object){}"}, "where": ["123", "132"]}"""
case class Trim(source: Code, where: List[Int]) extends Intention

object Inspect extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val inspectFormats = jsonFormat2(Inspect.apply)
}

object Remove extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val deleteFormats = jsonFormat3(Remove.apply)
}

object Rename extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val renameFormats = jsonFormat4(Rename.apply)
}

object Optimize extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val optimizeFormats = jsonFormat1(Optimize.apply)
}

object Format extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val formatFormats = jsonFormat1(Format.apply)
}

object Deduplicate extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val deduplicateFormats = jsonFormat1(Deduplicate.apply)
}

object Cleanup extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val cleanupFormats = jsonFormat1(Cleanup.apply)
}

object Publish extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val publishFormats = jsonFormat1(Publish.apply)
}

object Persist extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val persistFormats = jsonFormat1(Persist.apply)
}

object Trim extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val clipFormats = jsonFormat2(Trim.apply)
}
