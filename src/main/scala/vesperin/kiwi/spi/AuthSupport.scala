package vesperin.kiwi.spi

import spray.routing.AuthenticationFailedRejection.CredentialsMissing
import spray.routing.authentication._
import spray.routing.{AuthenticationFailedRejection, HttpService, RequestContext}
import vesperin.kiwi.config.Configuration

import scala.concurrent.{ExecutionContext, Future}

case class Role(id: Int, description: String)
case class Auth(userId: String, token: String)
case class Member(username:String, role: Int)
case class Membership(auth: Auth, role: Role)

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
trait AuthSupport extends Configuration {
  this: HttpService =>

  implicit def executionContext: ExecutionContext = actorRefFactory.dispatcher


  def createMembership(member: Member): Option[Membership] = {
    val pwd = getPassword(member.username)
    pwd match {
      case Some(x) =>
        val membership = Membership(
          Auth(member.username, x),
          Role(member.role, member.username)
        )
        Some(membership)
      case _=>
        None
    }
  }

  def doAuthenticate(username: String): Future[Option[Membership]] = {
    getMember(username) match  {
      case Some(x) => Future { createMembership(x) }
      case _=>  Future { None }
    }
  }

  def getMember(username: String) : Option[Member] = {
    if(club.contains(username)){
      Some(Member(username, club(username)))
    } else {
      None
    }
  }

  /* Extract the token from an HTTP Header or URL. */
  def getToken(ctx: RequestContext): Option[String] = {
    //This is needed in case the auth_token is passed as a GET parameter.
    //It's up to you to remove this part of code or not.
    val query = ctx.request.uri.query.get("auth_token")
    if (query.isDefined)
      Some(query.get)
    else {
      val header = ctx.request.headers.find(_.name == "x-auth-token")
      header.map { _.value }
    }
  }

  private def getPassword(username: String): Option[String] = {
    if(passwords.contains(username)) {
      Some(passwords(username))
    } else {
      None
    }
  }


  def isCurator(membership: Membership): Boolean = {
    membership.role.id == 0
  }

  def isReviewer(membership: Membership): Boolean = {
    membership.role.id == 0 || membership.role.id == 1
  }

  def membership: RequestContext => Future[Authentication[Membership]] = {
    ctx: RequestContext =>
      val token = getToken(ctx)
      if(token.isEmpty){
        Future(Left(AuthenticationFailedRejection(CredentialsMissing, List())))
      } else doAuthenticate(token.get).map {
        membership =>
          if (membership.isDefined)
            Right(membership.get)
          else
            Left(AuthenticationFailedRejection(CredentialsMissing, List()))
      }
  }
}
