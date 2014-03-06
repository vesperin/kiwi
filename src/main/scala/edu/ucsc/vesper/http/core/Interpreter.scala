package edu.ucsc.vesper.http.core

import edu.ucsc.refactor.{Vesper, Refactorer}
import edu.ucsc.vesper.http.config.Configuration
import akka.actor.Actor
import scala.concurrent.{ExecutionContext, Future}
import edu.ucsc.vesper.http.domain.LoungeObjects._
import scala.Some
import ExecutionContext.Implicits.global


/**
 * Message object for a request to inspect a source code.
 */
case class Curate(who:Auth, command:Command)
/**
 * Message object for a question for Vesper.
 */
case class Get(who:Role, question: String)

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
trait Interpreter extends Configuration {
  val refactorer: Refactorer = Vesper.createRefactorer()

  val Curator     = 0
  val Reviewer    = 1

  def ask(who:Role, question:String): Future[Option[Answer]] = {

    def f(x: Int): Boolean = if(x == Reviewer) true else false
    def g(x: Int): Boolean = if(x == Curator)  true else false

    if(who.id != Curator) return Future { None }

    question  match {
      case "curators"   => Future { Some(Answer(club.filter {case (k,v) => g(v)}.keySet.toList)) }
      case "reviewers"  => Future { Some(Answer(club.filter {case (k,v) => f(v)}.keySet.toList)) }
      case "everybody"  => Future { Some(Answer(passwords.keySet.toList)) }
    }
  }
}

class InterpreterActor extends Actor with Interpreter {
  override def receive = {
    case Get(who, question)   => sender ! ask(who, question)
//    case Curate(who, command) => sender ! eval(who, command)
    case _=> None
  }
}
