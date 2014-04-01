package edu.ucsc.vesper.http.domain

import spray.json.DefaultJsonProtocol
import spray.httpx.SprayJsonSupport

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
object LoungeObjects {
  case class Role(id: Int, description: String)
  case class Auth(userId: String, token: String)
  case class Member(username:String, role: Int)
  case class Membership(auth: Auth, role: Role)

  case class Comment(
        id: Option[String]        = None,
        username: Option[String]  = None,
        text: String,
        mark: Option[List[Int]]   = None
  )

  case class Code(
       id: Option[String]                               = None,
       name: String,
       description: String,
       version: Option[String]                          = None,
       content: String, comments: Option[List[Comment]] = None
  )

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


  case class Warning(name: String, description: String, where: Option[List[Int]])
  case class Failure(message: String)
  case class Info(messages: List[String])

  case class Answer(items: List[String])

  // the result of every refactoring
  case class ChangeSummary(
        draft: Option[Draft]            = None,
        info: Option[Info]              = None,
        warnings: Option[List[Warning]] = None,
        failure: Option[Failure]        = None
  )

  case class Command(
        inspect: Option[Inspect]          = None,
        remove:  Option[Remove]           = None,
        rename:  Option[Rename]           = None,
        optimize: Option[Optimize]        = None,
        format: Option[Format]            = None,
        deduplicate: Option[Deduplicate]  = None,
        cleanup: Option[Cleanup]          = None,
        publish: Option[Publish]          = None
  )


  object Comment extends DefaultJsonProtocol with SprayJsonSupport {
    implicit val commentFormats = jsonFormat4(Comment.apply)
  }

  object Code extends DefaultJsonProtocol with SprayJsonSupport {
    implicit val codeFormats = jsonFormat6(Code.apply)
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

  object ChangeSummary extends DefaultJsonProtocol with SprayJsonSupport {
    implicit val changeSummaryFormats = jsonFormat4(ChangeSummary.apply)
  }

  object Command extends DefaultJsonProtocol with SprayJsonSupport {
    implicit val requestFormats = jsonFormat8(Command.apply)
  }
}
