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
  // """{ "what": "class", "name":"StringUtil", "range": ["123", "132"] }"""
  case class Delete(what: String, name: String, range: List[Int])

  case class Request(
        inspect: Option[Inspect]  = None,
        delete: Option[Delete]    = None
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

  object Delete extends DefaultJsonProtocol with SprayJsonSupport {
    implicit val deleteFormats = jsonFormat3(Delete.apply)
  }

  object Request extends DefaultJsonProtocol with SprayJsonSupport {
    implicit val requestFormats = jsonFormat2(Request.apply)
  }
}
