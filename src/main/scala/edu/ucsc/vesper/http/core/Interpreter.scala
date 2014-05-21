package edu.ucsc.vesper.http.core

import edu.ucsc.refactor._
import edu.ucsc.vesper.http.config.Configuration
import edu.ucsc.vesper.http.domain.Models._
import spray.http.DateTime
import scala.collection.mutable
import edu.ucsc.refactor.util.{CommitHistory, CommitPublisher, Commit}
import edu.ucsc.refactor.spi._
import scala.Some
import edu.ucsc.vesper.http.domain.Models.Auth
import edu.ucsc.vesper.http.domain.Models.Role
import scala.collection.JavaConversions._
import scala.concurrent.{ExecutionContext, Future}

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
trait Interpreter extends Configuration with VesperConversions with CommandFlattener {

  implicit def executionContext = ExecutionContext.Implicits.global

  val Curator     = 0
  val Reviewer    = 1

  def ask(who:Role, question:String): Future[Option[Answer]] = {

    def f(x: Int): Boolean = if(x == Reviewer) true else false
    def g(x: Int): Boolean = if(x == Curator)  true else false

    if(who.id != Curator) return Future{Some(Answer(List("You are not authorized to see these resources.")))}

    question  match {
      case "curators"   => Future{Some(Answer(club.filter {case (k,v) => g(v)}.keySet.toList))}
      case "reviewers"  => Future{Some(Answer(club.filter {case (k,v) => f(v)}.keySet.toList))}
      case "everybody"  => Future{Some(Answer(passwords.keySet.toList))}
    }
  }


  private def evalInspect(refactorer: Refactorer, inspect: Inspect): Option[ChangeSummary] = {
    try {
      val vesperSource: Source = asSource(inspect.source)

      val issues:mutable.Set[Issue]         = asScalaSet(refactorer.detectIssues(vesperSource))
      var warnings:mutable.Buffer[Warning]  = mutable.Buffer.empty[Warning]

      for(i <- issues){
        warnings += asWarning(vesperSource, i)
      }

      val result = if(!warnings.isEmpty){
        Some(ChangeSummary(warnings = Some(warnings.toList)))
      } else {
        var messages:mutable.Buffer[String]  = mutable.Buffer.empty[String]
        messages += "It looks clear!"
        Some(ChangeSummary(info = Some(Info(messages.toList))))
      }

      result
    } catch {
      case e: Exception =>
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

  private def removeMember(refactorer:Refactorer, what: String, where: List[Int], source: Source): Option[ChangeSummary] = {
    var result: Option[ChangeSummary] = None
    try {
      val select: SourceSelection = new SourceSelection(source, where(0), where(1))
      val request: ChangeRequest  = createRemoveChangeRequest(what, select)

      val change: Change          = refactorer.createChange(request)
      val commit: Commit          = refactorer.apply(change)

      commit != null && commit.isValidCommit match {
        case true =>  result = Some(ChangeSummary(draft = Some(asDraft(commit))))
        case false => result = Some(ChangeSummary(failure = Some(Failure(change.getErrors.mkString(" ")))))
      }

    } catch {
      case e: Exception =>
        return Some(ChangeSummary(failure = Some(Failure(e.getMessage))))
    }

    result
  }

  private def evalRemove(refactorer: Refactorer, delete: Remove): Option[ChangeSummary] = {
    val what:String           = delete.what
    val where:List[Int]       = delete.where
    val vesperSource: Source  = asSource(delete.source)

    removeMember(refactorer, what, where, vesperSource)
  }

  private def createRenameChangeRequest(what: String, name: String, selection: SourceSelection): ChangeRequest = {
    what match {
      case "class"      => ChangeRequest.renameClassOrInterface(selection, name)
      case "method"     => ChangeRequest.renameMethod(selection, name)
      case "field"      => ChangeRequest.renameField(selection, name)
      case "parameter"  => ChangeRequest.renameParameter(selection, name)
      case "member"     => ChangeRequest.renameSelectedMember(selection, name)
      case _=> throw new NoSuchElementException(what + " was not found")
    }
  }

  private def renameMember(refactorer: Refactorer, what: String, name: String, where: List[Int], source: Source): Option[ChangeSummary] = {
    var result: Option[ChangeSummary] = None
    try {
      val select: SourceSelection = new SourceSelection(source, where(0), where(1))
      val request: ChangeRequest  = createRenameChangeRequest(what, name, select)

      val change: Change          = refactorer.createChange(request)
      val commit: Commit          = refactorer.apply(change)

      commit != null && commit.isValidCommit match {
        case true =>  result = Some(ChangeSummary(draft = Some(asDraft(commit))))
        case false => result = Some(ChangeSummary(failure = Some(Failure(change.getErrors.mkString(" ")))))
      }

    } catch {
      case e: Exception => return Some(ChangeSummary(failure = Some(Failure(e.getMessage))))
    }

    result
  }

  private def evalRename(refactorer: Refactorer, rename: Rename): Option[ChangeSummary] = {
    val what:String           = rename.what
    val where:List[Int]       = rename.where
    val name: String          = rename.to
    val vesperSource: Source  = asSource(rename.source)

    renameMember(refactorer, what, name, where, vesperSource)
  }

  private def evalOptimize(refactorer: Refactorer, optimize: Optimize): Option[ChangeSummary] = {
    var result: Option[ChangeSummary] = None

    val vesperSource: Source  = asSource(optimize.source)

    val request:ChangeRequest   = ChangeRequest.optimizeImports(vesperSource)
    val change: Change          = refactorer.createChange(request)
    val commit: Commit          = refactorer.apply(change)

    commit != null && commit.isValidCommit match {
      case true =>  result = Some(ChangeSummary(draft = Some(asDraft(commit))))
      case false => result = Some(ChangeSummary(failure = Some(Failure(change.getErrors.mkString(" ")))))
    }

    result
  }

  private def evalFormat(refactorer: Refactorer, format: Format): Option[ChangeSummary] = {
    var result: Option[ChangeSummary] = None

    val vesperSource: Source  = asSource(format.source)

    val request:ChangeRequest   = ChangeRequest.reformatSource(vesperSource)
    val change: Change          = refactorer.createChange(request)
    val commit: Commit          = refactorer.apply(change)

    commit != null && commit.isValidCommit match {
      case true =>  result = Some(ChangeSummary(draft = Some(asFormattedDraft(commit))))
      case false => result = Some(ChangeSummary(failure = Some(Failure(change.getErrors.mkString(" ")))))
    }

    result
  }

  private def evalCleanup(refactorer: Refactorer, cleanup: Cleanup): Option[ChangeSummary] = {
    var result: Option[ChangeSummary] = None

    val vesperSource: Source        = asSource(cleanup.source)
    val queue:mutable.Queue[Source] = new mutable.Queue[Source]

    queue += vesperSource


    def createCommit(refactorer: Refactorer, issue: Issue): Commit = {
      val change: Change = refactorer.createChange(ChangeRequest.forIssue(issue))
      val commit: Commit = refactorer.apply(change)

      commit
    }


    val stack:mutable.Stack[Commit] = new mutable.Stack[Commit]

    while(!queue.isEmpty){
      val code: Source = queue.dequeue()
      val issues:mutable.Set[Issue] = asScalaSet(refactorer.detectIssues(code))

      for(i <- issues){
        val name: Smell = i.getName
        if (Names.hasAvailableResponse(name)) { // the assumption is there is ONE instance of DEDUPLICATE issue
          if (Names.from(i.getName).isSame(Refactoring.DEDUPLICATE) && queue.isEmpty) {

            val commit: Commit = createCommit(refactorer, i)

            if(commit != null){
              if(commit.isValidCommit){

                stack.push(commit)

                queue   += commit.getSourceAfterChange
              }
            }
          } else if(Names.from(i.getName).isSame(Refactoring.DELETE_UNUSED_IMPORTS) && queue.isEmpty){

            val commit: Commit = createCommit(refactorer, i)

            if(commit != null){
              if(commit.isValidCommit){

                stack.push(commit)

                queue += commit.getSourceAfterChange
              }
            }
          }
        }
      }

    }


    result = if (!stack.isEmpty) {
      Some(
        ChangeSummary(
          draft = Some(
            asFormattedDraft(
              stack.pop(),
              cause       = "Full cleanup",
              description = "Reformatted code and also removed code redundancies"
            )
          )
        )
      )
    } else {  // at least format the code
      val request:ChangeRequest   = ChangeRequest.reformatSource(vesperSource)
      val change: Change          = refactorer.createChange(request)
      val commit: Commit          = refactorer.apply(change)

      var reformatResult: Option[ChangeSummary] = None

      commit != null && commit.isValidCommit match {
        case true =>
          reformatResult = Some(
            ChangeSummary(
              draft = Some(
                asFormattedDraft(
                  commit,
                  cause       = "Full cleanup",
                  description = "Reformatted code and also removed code redundancies"
                )
              )
            )
          )
        case false => result = Some(ChangeSummary(failure = Some(Failure(change.getErrors.mkString(" ")))))
      }

      reformatResult
    }

    result
  }

  private def evalDeduplicate(refactorer: Refactorer, deduplicate: Deduplicate): Option[ChangeSummary] = {
    var result: Option[ChangeSummary] = None

    val vesperSource: Source      = asSource(deduplicate.source)
    val issues:mutable.Set[Issue] = asScalaSet(refactorer.detectIssues(vesperSource))

    for(i <- issues){
      val name: Smell = i.getName
      if (Names.hasAvailableResponse(name)) { // the assumption is there is ONE instance of DEDUPLICATE issue
        if (Names.from(i.getName).isSame(Refactoring.DEDUPLICATE)) {
          val change: Change = refactorer.createChange(ChangeRequest.forIssue(i))
          val commit: Commit = refactorer.apply(change)

          commit != null && commit.isValidCommit match {
            case true =>  result = Some(ChangeSummary(draft = Some(asDraft(commit))))
            case false => result = Some(ChangeSummary(failure = Some(Failure(change.getErrors.mkString(" ")))))
          }
        }
      }
    }

    result
  }


  private def evalPublish(who:Auth, publish: Publish): Option[ChangeSummary] = {
    try {

      val commitHistory: CommitHistory = new CommitHistory()

      for(d <- publish.drafts){
        commitHistory.add(asCommit(who.userId, d))
      }

      val p: CommitPublisher = new CommitPublisher(
        commitHistory,
        new Credential(who.userId, who.token)
      )

      var result: Option[ChangeSummary]   = None
      val commits:mutable.Buffer[Commit]  = asScalaBuffer(p.publish())
      val commit: Commit                  = commits.last

      commit != null && commit.isValidCommit match {
        case true =>  result = Some(ChangeSummary(draft = Some(asDraft(commit))))
        case false => result = Some(ChangeSummary(failure = Some(Failure("Unable to publish commit"))))
      }

      result
    } catch {
      case e: Exception => Some(ChangeSummary(failure = Some(Failure(e.getMessage))))
    }
  }

  def eval(who:Auth, command: Command): Future[Option[ChangeSummary]] = {

    val environment: Refactorer = Vesper.createRefactorer()

    println(who.userId + " is curating at " + DateTime.now + "\n")

    val answer = flatten(command)

    answer match {
      case inspect:Inspect          => Future{evalInspect(environment, inspect)}
      case remove:Remove            => Future{evalRemove(environment, remove)}
      case rename:Rename            => Future{evalRename(environment, rename)}
      case optimize:Optimize        => Future{evalOptimize(environment, optimize)}
      case format:Format            => Future{evalFormat(environment, format)}
      case deduplicate:Deduplicate  => Future{evalDeduplicate(environment, deduplicate)}
      case cleanup:Cleanup          => Future{evalCleanup(environment, cleanup)}
      case publish:Publish          => Future{evalPublish(who, publish)}
      case _                        => Future{Some(ChangeSummary(failure = Some(Failure("Unknown command!"))))}
    }
  }
}


case class VesperInterpreter() extends Interpreter

