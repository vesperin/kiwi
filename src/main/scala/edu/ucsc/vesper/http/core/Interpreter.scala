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
import edu.ucsc.refactor.util.{Commit, Locations}
import edu.ucsc.refactor.spi.{Refactoring, Names, Smell}

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

    if(who.id != Curator) return Some(Answer(List("You are not authorized to see these resources.")))

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
        Some(ChangeSummary(failure = Some(Failure(e.getMessage))))
    }

  }

  private def createRemoveChangeRequest(what: String, selection: SourceSelection): ChangeRequest = {
    what match {
      case "class"      => ChangeRequest.deleteClass(selection)
      case "method"     => ChangeRequest.deleteMethod(selection)
      case "field"      => ChangeRequest.deleteField(selection)
      case "parameter"  => ChangeRequest.deleteParameter(selection)
      case "region"     => ChangeRequest.deleteRegion(selection)
      case _=> throw new NoSuchElementException(what + " was not found")
    }
  }

  private def fromSrcToCode(source: Source): Code = {

    val sourceId: Option[String]  = if (source.getId != null) Some(source.getId) else None
    val sourceName: String        = source.getName
    val sourceDesc: String        = source.getDescription
    val sourceCont: String        = source.getContents

    val srcComments: Option[List[Comment]] = if(source.getNotes.isEmpty) None else {
      val itr: java.util.Iterator[Note] = source.getNotes.iterator

      var allComments:mutable.Buffer[Comment] = mutable.Buffer.empty[Comment]
      while(itr.hasNext){
        val each: Note = itr.next
        val nodeId: Option[String]    = if(each.getId == null) None else Some(each.getId)
        val username: Option[String]  = if(each.getUser == null) None else Some(each.getUser)
        val mark:Option[List[Int]]    = if(each.getMark == null) None else {
          Some(List(each.getMark.getStart.getOffset, each.getMark.getEnd.getOffset))
        }

        allComments += Comment(nodeId, username, each.getContent, mark)
      }

      Some(allComments.toList)
    }

    Code(sourceId, sourceName, sourceDesc, sourceCont, srcComments)
  }

  private def removeMember(what: String, where: List[Int], source: Source): Option[ChangeSummary] = {
    var result: Option[ChangeSummary] = None
    try {
      val select: SourceSelection = new SourceSelection(source, where(0), where(1))
      val request: ChangeRequest  = createRemoveChangeRequest(what, select)

      val change: Change          = refactorer.createChange(request)
      val commit: Commit          = refactorer.apply(change)

      if(commit != null){
        if(commit.isValidCommit){
          val msg:String      = commit.getNameOfChange.getKey + ": " + commit.getNameOfChange.getSummary
          val updated: Source = commit.getSourceAfterChange

          result = Some(ChangeSummary(edit = Some(Edit(msg, fromSrcToCode(updated)))))
        }
      }
    } catch {
      case e: Throwable => return Some(ChangeSummary(failure = Some(Failure(e.getMessage))))
    }

    result
  }

  private def evalRemove(delete: Remove): Option[ChangeSummary] = {
    val what:String           = delete.what
    val where:List[Int]       = delete.where
    val vesperSource: Source  = fromCodeToSrc(delete.source)

    removeMember(what, where, vesperSource)
  }

  private def createRenameChangeRequest(what: String, name: String, selection: SourceSelection): ChangeRequest = {
    what match {
      case "class"      => ChangeRequest.deleteClass(selection)
      case "method"     => ChangeRequest.deleteMethod(selection)
      case "field"      => ChangeRequest.deleteField(selection)
      case "parameter"  => ChangeRequest.deleteParameter(selection)
      case "region"     => ChangeRequest.deleteRegion(selection)
      case _=> throw new NoSuchElementException(what + " was not found")
    }
  }

  private def renameMember(what: String, name: String, where: List[Int], source: Source): Option[ChangeSummary] = {
    var result: Option[ChangeSummary] = None
    try {
      val select: SourceSelection = new SourceSelection(source, where(0), where(1))
      val request: ChangeRequest  = createRenameChangeRequest(what, name, select)

      val change: Change          = refactorer.createChange(request)
      val commit: Commit          = refactorer.apply(change)

      if(commit != null){
        if(commit.isValidCommit){
          val msg:String      = commit.getNameOfChange.getKey + ": " + commit.getNameOfChange.getSummary
          val updated: Source = commit.getSourceAfterChange

          result = Some(ChangeSummary(edit = Some(Edit(msg, fromSrcToCode(updated)))))
        }
      }
    } catch {
      case e: Throwable => return Some(ChangeSummary(failure = Some(Failure(e.getMessage))))
    }

    result
  }

  private def evalRename(rename: Rename): Option[ChangeSummary] = {
    val what:String           = rename.what
    val where:List[Int]       = rename.where
    val name: String          = rename.to
    val vesperSource: Source  = fromCodeToSrc(rename.source)

    renameMember(what, name, where, vesperSource)
  }

  private def evalOptimize(optimize: Optimize): Option[ChangeSummary] = {
    var result: Option[ChangeSummary] = None

    val vesperSource: Source  = fromCodeToSrc(optimize.source)

    val request:ChangeRequest   = ChangeRequest.optimizeImports(vesperSource)
    val change: Change          = refactorer.createChange(request)
    val commit: Commit          = refactorer.apply(change)

    if(commit != null){
      if(commit.isValidCommit){
        val msg:String      = commit.getNameOfChange.getKey + ": " + commit.getNameOfChange.getSummary
        val updated: Source = commit.getSourceAfterChange

        result = Some(ChangeSummary(edit = Some(Edit(msg, fromSrcToCode(updated)))))
      }
    }

    result
  }

  private def evalFormat(format: Format): Option[ChangeSummary] = {
    var result: Option[ChangeSummary] = None

    val vesperSource: Source  = fromCodeToSrc(format.source)

    val request:ChangeRequest   = ChangeRequest.reformatSource(vesperSource)
    val change: Change          = refactorer.createChange(request)
    val commit: Commit          = refactorer.apply(change)

    if(commit != null){
      if(commit.isValidCommit){
        val msg:String      = commit.getNameOfChange.getKey + ": " + commit.getNameOfChange.getSummary
        val updated: Source = commit.getSourceAfterChange

        result = Some(ChangeSummary(edit = Some(Edit(msg, fromSrcToCode(updated)))))
      }
    }

    result
  }

  private def evalDeduplicate(deduplicate: Deduplicate): Option[ChangeSummary] = {
    var result: Option[ChangeSummary] = None

    val vesperSource: Source  = fromCodeToSrc(deduplicate.source)

    val issues:java.util.Set[Issue]   = refactorer.detectIssues(vesperSource)
    val itr:java.util.Iterator[Issue] = issues.iterator()

    while(itr.hasNext){
      val each:Issue = itr.next
      val name: Smell = each.getName
      if (Names.hasAvailableResponse(name)) {
        if (Names.from(each.getName).isSame(Refactoring.DEDUPLICATE)) {
          val change: Change = refactorer.createChange(ChangeRequest.forIssue(each))
          val commit: Commit = refactorer.apply(change)

          if(commit != null){
            if(commit.isValidCommit){
              val msg:String      = commit.getNameOfChange.getKey + ": " + commit.getNameOfChange.getSummary
              val updated: Source = commit.getSourceAfterChange

              result = Some(ChangeSummary(edit = Some(Edit(msg, fromSrcToCode(updated)))))
            }
          }
        }
      }
    }

    result
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
      case inspect:Inspect    => return evalInspect(inspect)
      case remove:Remove      => return evalRemove(remove)
      case rename:Rename      => return evalRename(rename)
      case optimize:Optimize  => return evalOptimize(optimize)
      case format:Format      => return evalFormat(format)
      case deduplicate:Deduplicate  => return evalDeduplicate(deduplicate)
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
