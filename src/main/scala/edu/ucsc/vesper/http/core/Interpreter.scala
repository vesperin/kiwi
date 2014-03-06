package edu.ucsc.vesper.http.core

import edu.ucsc.refactor._
import edu.ucsc.vesper.http.config.Configuration
import akka.actor.Actor
import edu.ucsc.vesper.http.domain.LoungeObjects._
import spray.http.DateTime
import scala.collection.mutable
import org.eclipse.jdt.core.dom.{MethodDeclaration, ASTNode}
import edu.ucsc.vesper.http.domain.LoungeObjects.Role
import scala.Some
import edu.ucsc.vesper.http.domain.LoungeObjects.Auth
import edu.ucsc.refactor.util.Locations

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

  def ask(who:Role, question:String): Option[Answer] = {

    def f(x: Int): Boolean = if(x == Reviewer) true else false
    def g(x: Int): Boolean = if(x == Curator)  true else false

    if(who.id != Curator) return None

    question  match {
      case "curators"   => Some(Answer(club.filter {case (k,v) => g(v)}.keySet.toList))
      case "reviewers"  => Some(Answer(club.filter {case (k,v) => f(v)}.keySet.toList))
      case "everybody"  => Some(Answer(passwords.keySet.toList))
    }
  }

  private def fromCodeToSrc(source: Code): Source = {
    val result: Source = new Source(source.name, source.content, source.description)

    if(source.id != None){
      result.setId(source.id.get)
    }

    if(source.comments != None){
      val list = source.comments.get

      for(c <- list){
        val id: String        = if(c.id != None) c.id.get else null
        val username: String  = if(c.username != None) c.username.get else null
        val eachNote: Note    = new Note(id, username, c.text)

        result.addNote(eachNote)
      }
    }

    result
  }

  private def evalInspect(inspect: Inspect): Option[ChangeSummary] = {
    try {
      val vesperSource: Source = fromCodeToSrc(inspect.source)

      val issues:java.util.Set[Issue]   = refactorer.detectIssues(vesperSource)
      val itr:java.util.Iterator[Issue] = issues.iterator()

      var warnings:mutable.Buffer[Warning] = mutable.Buffer.empty[Warning]

      while(itr.hasNext){
        val each:Issue = itr.next
        var marks:mutable.Buffer[Int]         = mutable.Buffer.empty[Int]
        val nodes:java.util.List[ASTNode]     = each.getAffectedNodes
        val nitr:java.util.Iterator[ASTNode]  = nodes.iterator()

        while(nitr.hasNext){
          val node: ASTNode = nitr.next
          if(nodes.size() > 1){
            if(!node.isInstanceOf[MethodDeclaration]){
              val loc: Location = Locations.locate(vesperSource, node)
              marks += loc.getStart.getOffset
              marks += loc.getEnd.getOffset
            }
          } else {
            val loc: Location = Locations.locate(vesperSource, node)
            marks += loc.getStart.getOffset
            marks += loc.getEnd.getOffset
          }
        }

        warnings += Warning(each.getName.getKey, each.getName.getSummary, Some(marks.toList))

      }

      Some(ChangeSummary(warnings = Some(warnings.toList)))
    } catch {
      case e: Throwable =>
        println(e)
        None
    }

  }

  def eval(who:Auth, command: Command): Option[ChangeSummary] = {

    val answer = List(
      command.inspect,
      command.remove,
      command.rename,
      command.optimize,
      command.format,
      command.deduplicate
    ).flatten

    println(who.userId + " is curating at " + DateTime.now + "\n")

    answer(0) match {
      case inspect:Inspect => return evalInspect(inspect)
    }

    None
  }
}

class InterpreterActor extends Actor with Interpreter {
  override def receive = {
    case Get(who, question)   => sender ! ask(who, question)
    case Curate(who, command) => sender ! eval(who, command)
    case _=> None
  }
}
