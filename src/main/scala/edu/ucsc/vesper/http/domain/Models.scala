package edu.ucsc.vesper.http.domain

import spray.json.DefaultJsonProtocol
import spray.httpx.SprayJsonSupport
import sprest.models.{ModelCompanion, Model}

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
object Models {
  case class Role(id: Int, description: String)
  case class Auth(userId: String, token: String)
  case class Member(username:String, role: Int)
  case class Membership(auth: Auth, role: Role)

  case class Comment (
      /** location's lower bound; e.g., "line;col;offset" **/
      from: String

      /** location's upper bound; e.g., "line;col;offset" **/
      , to: String

      /** the comment's text **/
      , text: String

      /** the comment's author **/
      , username: Option[String]  = Some("You")
  )

  case class Code (
      /** the name of the source code (e.g., filename) **/
      name: String

      /** a piece of text describing this source code **/
      , description: String

      /** the current version of this source code **/
      , version: Option[String]  = None

      /** the current content of this Source **/
      , content: String

      /** tags or labels categorizing this source code **/
      , tags: List[String]       = List()

      /** the url where this Source was found **/
      , url: Option[String]      = None

      /** The date when this source code was found **/
      , birthday: Option[Long]   = None

      /** comments describing this source code. **/
      , comments: List[Comment]  = List()

      /** the code unique id **/
      , var id: Option[String]   = None
  ) extends Model[String]

  case class Draft(cause: String, description: String, timestamp: Long, before: Code, after: Code)

  // e.g., """{ "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"class Bootstrap {void inject(Object object){}"} }"""
  case class Inspect(source: Code)
  // """{ "what": "class", "where": ["123", "132"], "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"class Bootstrap {void inject(Object object){}"} }"""
  case class Remove(what: String, where: List[Int], source: Code)
  //"""{"rename" : { "what": "class", "where": [1, 2], "to":"ResourceInjector", "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"class Bootstrap {void inject(Object object){}"} }}"""
  case class Rename(what: String, where: List[Int], to: String, source: Code)
  // """{ "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"import java.util.List; \n public class Bootstrap {void inject(Object object){}"} }"""
  case class Optimize(source: Code)
  // """{ "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"public class Bootstrap {void inject(Object object){}"} }"""
  case class Format(source: Code)
  // """{ "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"public class Bootstrap {void inject(Object object){}"} }"""
  case class Deduplicate(source: Code)
  // """{ "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"public class Bootstrap {void inject(Object object){}"} }"""
  case class Cleanup(source: Code)
  // drafts' sources
  case class Publish(drafts: List[Draft])
  // """{ "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"public class Bootstrap {void inject(Object object){}"} }"""
  case class Persist(source: Code)

  // Find(any: Option[], all: Option[] ...
  case class Any(name: String, targets: List[String])
  case class Exact(name: String, value: String)
  case class ExactlyAll(name: String, targets: List[String])
  case class All(symbol: String = "*")
  case class Find(
        all: Option[All]              = None,
        any: Option[Any]              = None,
        exact: Option[Exact]          = None,
        exactlyAll:Option[ExactlyAll] = None,
        roles: Option[String]         = None
  )

  case class Warning(name: String, description: String, where: Option[List[Int]])
  case class Failure(message: String)
  case class Info(messages: List[String])

  case class Answer(items: List[String])

  // the result returned by Vesper; a refactoring or results request
  case class Result(
        draft: Option[Draft]            = None,
        info: Option[Info]              = None,
        warnings: Option[List[Warning]] = None,
        failure: Option[Failure]        = None,
        sources: Option[List[Code]]     = None
  )

  // the command to request some result
  case class Command(
        inspect: Option[Inspect]          = None,
        remove:  Option[Remove]           = None,
        rename:  Option[Rename]           = None,
        optimize: Option[Optimize]        = None,
        format: Option[Format]            = None,
        deduplicate: Option[Deduplicate]  = None,
        cleanup: Option[Cleanup]          = None,
        publish: Option[Publish]          = None,
        persist: Option[Persist]          = None,
        find: Option[Find]                = None
  )


  object Comment extends DefaultJsonProtocol with SprayJsonSupport {
    implicit val commentFormats = jsonFormat4(Comment.apply)
  }

  object Code extends ModelCompanion[Code, String] {
    implicit val codeFormats = jsonFormat9(Code.apply _)
  }

  object Draft extends DefaultJsonProtocol with SprayJsonSupport {
    implicit val codeFormats = jsonFormat5(Draft.apply)
  }

  object Inspect extends DefaultJsonProtocol with SprayJsonSupport {
    implicit val inspectFormats = jsonFormat1(Inspect.apply)
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

  object All extends DefaultJsonProtocol with SprayJsonSupport {
    implicit val allFormat = jsonFormat1(All.apply)
  }

  object Any extends DefaultJsonProtocol with SprayJsonSupport {
    implicit val anyFormat = jsonFormat2(Any.apply)
  }

  object Exact extends DefaultJsonProtocol with SprayJsonSupport {
    implicit val exactFormat = jsonFormat2(Exact.apply)
  }

  object ExactlyAll extends DefaultJsonProtocol with SprayJsonSupport {
    implicit val exactlyAllFormat = jsonFormat2(ExactlyAll.apply)
  }

  object Find extends DefaultJsonProtocol with SprayJsonSupport {
    implicit val findFormats = jsonFormat5(Find.apply)
  }

  object Persist extends DefaultJsonProtocol with SprayJsonSupport {
    implicit val persistFormats = jsonFormat1(Persist.apply)
  }

  object Warning extends DefaultJsonProtocol with SprayJsonSupport {
    implicit val failureFormats = jsonFormat3(Warning.apply)
  }

  object Failure extends DefaultJsonProtocol with SprayJsonSupport {
    implicit val failureFormats = jsonFormat1(Failure.apply)
  }

  object Info extends DefaultJsonProtocol with SprayJsonSupport {
    implicit val failureFormats = jsonFormat1(Info.apply)
  }

  object Answer extends DefaultJsonProtocol with SprayJsonSupport {
    implicit val answerFormats = jsonFormat1(Answer.apply)
  }

  object Result extends DefaultJsonProtocol with SprayJsonSupport {
    implicit val changeSummaryFormats = jsonFormat5(Result.apply)
  }

  object Command extends DefaultJsonProtocol with SprayJsonSupport {
    implicit val requestFormats = jsonFormat10(Command.apply)
  }
}
