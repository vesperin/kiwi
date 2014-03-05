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

  case class Comment(text: String)
  case class Code(
        id: Option[String] = None,
        name: String,
        description: String,
        content: String, comments: Option[List[Comment]] = None
  )

  // e.g., """{ "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"class Bootstrap {void inject(Object object}{}"} }"""
  case class Inspect(source: Code)
  // """{ "what": "class", "range": ["123", "132"], "source": {"name": "Bootstrap.java", "description":"Resource Injector", "content":"class Bootstrap {void inject(Object object}{}"} }"""
  case class Remove(what: String, range: List[Int], source: Code)

  case class Rename(what: String, from: String, to: String, range: List[Int])

  case class Request(
        inspect: Option[Inspect]  = None,
        remove:  Option[Remove]   = None
  )


  object Comment extends DefaultJsonProtocol with SprayJsonSupport {
    implicit val commentFormats = jsonFormat1(Comment.apply)
  }

  object Code extends DefaultJsonProtocol with SprayJsonSupport {
    implicit val codeFormats = jsonFormat5(Code.apply)
  }

  object Inspect extends DefaultJsonProtocol with SprayJsonSupport {
    implicit val inspectFormats = jsonFormat1(Inspect.apply)
  }

  object Remove extends DefaultJsonProtocol with SprayJsonSupport {
    implicit val deleteFormats = jsonFormat3(Remove.apply)
  }

  object Request extends DefaultJsonProtocol with SprayJsonSupport {
    implicit val requestFormats = jsonFormat2(Request.apply)
  }
}
