package edu.ucsc.vesper.http.core

import edu.ucsc.refactor._
import edu.ucsc.vesper.http.config.Configuration
import akka.actor.Actor
import edu.ucsc.vesper.http.domain.LoungeObjects._
import spray.http.DateTime
import scala.collection.mutable
import edu.ucsc.refactor.util.{CommitHistory, CommitPublisher, Commit}
import edu.ucsc.refactor.spi._
import scala.Some
import edu.ucsc.vesper.http.domain.LoungeObjects.Auth
import edu.ucsc.vesper.http.domain.LoungeObjects.Role
import scala.collection.JavaConversions._

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
trait Interpreter extends Configuration with VesperConversions {
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


  private def evalInspect(inspect: Inspect): Option[ChangeSummary] = {
    try {
      val vesperSource: Source = asSource(inspect.source)

      val issues:mutable.Set[Issue]         = asScalaSet(refactorer.detectIssues(vesperSource))
      var warnings:mutable.Buffer[Warning]  = mutable.Buffer.empty[Warning]

      for(i <- issues){
        warnings += asWarning(vesperSource, i)
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

  private def removeMember(what: String, where: List[Int], source: Source): Option[ChangeSummary] = {
    var result: Option[ChangeSummary] = None
    try {
      val select: SourceSelection = new SourceSelection(source, where(0), where(1))
      val request: ChangeRequest  = createRemoveChangeRequest(what, select)

      val change: Change          = refactorer.createChange(request)
      val commit: Commit          = refactorer.apply(change)

      if(commit != null){
        if(commit.isValidCommit){
          result = Some(ChangeSummary(draft = Some(asDraft(commit))))
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
    val vesperSource: Source  = asSource(delete.source)

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
          result = Some(ChangeSummary(draft = Some(asDraft(commit))))
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
    val vesperSource: Source  = asSource(rename.source)

    renameMember(what, name, where, vesperSource)
  }

  private def evalOptimize(optimize: Optimize): Option[ChangeSummary] = {
    var result: Option[ChangeSummary] = None

    val vesperSource: Source  = asSource(optimize.source)

    val request:ChangeRequest   = ChangeRequest.optimizeImports(vesperSource)
    val change: Change          = refactorer.createChange(request)
    val commit: Commit          = refactorer.apply(change)

    if(commit != null){
      if(commit.isValidCommit){
        result = Some(ChangeSummary(draft = Some(asDraft(commit))))
      }
    }

    result
  }

  private def evalFormat(format: Format): Option[ChangeSummary] = {
    var result: Option[ChangeSummary] = None

    val vesperSource: Source  = asSource(format.source)

    val request:ChangeRequest   = ChangeRequest.reformatSource(vesperSource)
    val change: Change          = refactorer.createChange(request)
    val commit: Commit          = refactorer.apply(change)

    if(commit != null){
      if(commit.isValidCommit){
        result = Some(ChangeSummary(draft = Some(asDraft(commit))))
      }
    }

    result
  }

  private def evalDeduplicate(deduplicate: Deduplicate): Option[ChangeSummary] = {
    var result: Option[ChangeSummary] = None

    val vesperSource: Source      = asSource(deduplicate.source)
    val issues:mutable.Set[Issue] = asScalaSet(refactorer.detectIssues(vesperSource))

    for(i <- issues){
      val name: Smell = i.getName
      if (Names.hasAvailableResponse(name)) { // the assumption is there is ONE instance of DEDUPLICATE issue
        if (Names.from(i.getName).isSame(Refactoring.DEDUPLICATE)) {
          val change: Change = refactorer.createChange(ChangeRequest.forIssue(i))
          val commit: Commit = refactorer.apply(change)

          if(commit != null){
            if(commit.isValidCommit){
              result = Some(ChangeSummary(draft = Some(asDraft(commit))))
            }
          }
        }
      }
    }

    result
  }


  private def evalPublish(who:Auth, publish: Publish): Option[ChangeSummary] = {
    val commitHistory: CommitHistory = new CommitHistory()

    for(d <- publish.drafts){
      commitHistory.add(asCommit(who.userId, d))
    }

    val p: CommitPublisher = new CommitPublisher(
      commitHistory,
      new Credential(who.userId, who.token)
    )

    val commits:mutable.Buffer[Commit]  = asScalaBuffer(p.publish())
    val commit: Commit                  = commits.last

    if(commit == null || !commit.isValidCommit) {
      return Some(ChangeSummary(failure = Some(Failure(""))))
    }

    Some(ChangeSummary(draft = Some(asDraft(commit))))
  }

  def eval(who:Auth, command: Command): Option[ChangeSummary] = {

    val answer = List(
      command.inspect,
      command.remove,
      command.rename,
      command.optimize,
      command.format,
      command.deduplicate,
      command.publish
    ).flatten

    println(who.userId + " is curating at " + DateTime.now + "\n")

    answer(0) match {
      case inspect:Inspect          => return evalInspect(inspect)
      case remove:Remove            => return evalRemove(remove)
      case rename:Rename            => return evalRename(rename)
      case optimize:Optimize        => return evalOptimize(optimize)
      case format:Format            => return evalFormat(format)
      case deduplicate:Deduplicate  => return evalDeduplicate(deduplicate)
      case publish:Publish          => return evalPublish(who, publish)
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
