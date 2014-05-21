package edu.ucsc.vesper.http.core

import edu.ucsc.vesper.http.domain.Models._
import scala.concurrent.{Future, ExecutionContext}
import edu.ucsc.vesper.http.database.DatabaseSupport.CodeSnippets

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
trait Storage extends CommandFlattener {

  implicit def executionContext = ExecutionContext.Implicits.global


  private def evalPersist(persist: Persist): Future[Option[ChangeSummary]] = CodeSnippets.add(persist.source).flatMap { code =>
    Future { Some(ChangeSummary(info = Some(Info(List("%s was successfully stored on the server!".format(code.name)))))) }
  }

  def persist(command: Command): Future[Option[ChangeSummary]] = {
    val answer = flatten(command)
    answer match {
      case persist:Persist          => evalPersist(persist)
      case _                        => communicateFailure("Oh snap! Unable to recognize the given command")
    }
  }

  def findAll()                         = CodeSnippets.findAll()
  def findBySingleTag(tag: String)      = CodeSnippets.forTag(tag)
  def findByAllTags(tags: List[String]) = CodeSnippets.forAllTags(tags)
  def findByAnyTags(tags: List[String]) = CodeSnippets.forAnyOfTheseTags(tags)

  private def communicateFailure(message: String): Future[Option[ChangeSummary]] = {
    Future { Some(ChangeSummary(failure = Some(Failure(message)))) }
  }
}

case class VesperStorage() extends Storage
