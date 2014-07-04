package edu.ucsc.vesper.http.domain

import spray.httpx.SprayJsonSupport
import spray.json.DefaultJsonProtocol
import sprest.models.{Model, ModelCompanion}

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

      /** a piece of text describing this source code; i.e., the synopsis **/
      , description: String

      /** the current content of this Source **/
      , content: String

      /** the curation elapsed time **/
      , elapsedtime: Option[String]   = None

      /**
       * tags or labels categorizing this source code. This
       * information gives the context of source code; e.g., Cracking code interview.
       */
      , tags: List[String]            = List()

      /** a sub-category indicating the data structures used in this source code **/
      , datastructures: List[String]  = List()

      /** a sub-category indicating the algorithms used is this source code **/
      , algorithms: List[String]      = List()

      /**
       * The unique refactorings that were used that led to this transformed source code.
       * This may give us some insight of what was on the programmer's mind when changing
       * this source code.
       */
      , refactorings: List[String]    = List()

      /**
       * Level of confidence with respect to the quality of the final result (source code); e.g., stars?
       */
      , confidence: Int               = 0

      /** the url where this Source was found **/
      , url: Option[String]           = None

      /** The date when this source code was found **/
      , birthday: Option[Long]        = None

      /** comments describing this source code. **/
      , comments: List[Comment]       = List()

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

  case class Body(kind: String, id: String, longUrl: String)
  case class LongURL(longUrl: String)

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

  case class Warning(message: String)
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
    implicit val codeFormats = jsonFormat13(Code.apply)
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

  object Body extends DefaultJsonProtocol with SprayJsonSupport {
    implicit val bodyFormats = jsonFormat3(Body.apply)
  }

  object LongURL extends DefaultJsonProtocol with SprayJsonSupport {
    implicit val urlFormats = jsonFormat1(LongURL.apply)
  }

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

  object Persist extends DefaultJsonProtocol with SprayJsonSupport {
    implicit val persistFormats = jsonFormat1(Persist.apply)
  }

  object Warning extends DefaultJsonProtocol with SprayJsonSupport {
    implicit val failureFormats = jsonFormat1(Warning.apply)
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
