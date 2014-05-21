package edu.ucsc.vesper.http.core

import edu.ucsc.refactor.{Location, Issue, Note, Source}
import edu.ucsc.vesper.http.domain.Models.{Warning, Draft, Comment, Code}
import scala.collection.mutable
import edu.ucsc.refactor.util.{SourceFormatter, Locations, Commit}
import edu.ucsc.refactor.spi.{CommitSummary, Refactoring, Name}
import java.util.Date
import org.eclipse.jdt.core.dom.{VariableDeclaration, SimpleName, MethodDeclaration, ASTNode}

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
trait VesperConversions {
  def asCode(source: Source): Code = {
    val sourceId: Option[String]  = if (source.getId != null) Some(source.getId) else None
    val sourceName: String        = source.getName
    val sourceDesc: String        = source.getDescription
    val sourceCont: String        = source.getContents

    val srcComments: List[Comment] = if(source.getNotes.isEmpty) List() else {
      val itr: java.util.Iterator[Note] = source.getNotes.iterator

      var allComments:mutable.Buffer[Comment] = mutable.Buffer.empty[Comment]
      while(itr.hasNext){
        val each: Note = itr.next
        val isMarkEmpty: Boolean  = each.getMark == null

        val from: String = if(isMarkEmpty) "0;0;0" else {
          val line: Int = each.getMark.getStart.getLine
          val col: Int  = each.getMark.getStart.getColumn
          val off: Int  = each.getMark.getStart.getOffset

          "%d;%d;%d".format(line, col, off)
        }

        val to: String = if(isMarkEmpty) "0;0;0" else {
          val line: Int = each.getMark.getEnd.getLine
          val col: Int  = each.getMark.getEnd.getColumn
          val off: Int  = each.getMark.getEnd.getOffset

          "%d;%d;%d".format(line, col, off)
        }

        allComments += Comment(from, to, each.getContent)
      }

      allComments.toList
    }

    Code(
      name        = sourceName,
      description = sourceDesc,
      content     = sourceCont,
      tags        = List(),
      comments    = srcComments,
      id          = sourceId
    )
  }

  def asSource(source: Code): Source = {
    val result: Source = new Source(source.name, source.content, source.description)

    if(source.id != None){
      result.setId(source.id.get)
    }

    if(source.comments != null){
      val list = source.comments

      for(c <- list){
        val id: String        = null // no id since comments are not persisted and are just embedded into the Code schema
        val username: String  = if(c.username != None) c.username.get else null
        val eachNote: Note    = new Note(id, username, c.text)

        result.addNote(eachNote)
      }
    }

    result
  }


  def asFormattedDraft(commit: Commit, cause: String, description: String): Draft = {
    val src: Source               = commit.getSourceAfterChange
    val formattedContent: String  = new SourceFormatter().format(src.getContents)

    Draft(
      cause,
      description,
      commit.getTimestamp,
      asCode(commit.getSourceBeforeChange),
      asCode(Source.from(src, formattedContent))
    )
  }

  def asFormattedDraft(commit: Commit): Draft = {
    asFormattedDraft(
      commit,
      commit.getNameOfChange.getKey,
      simplePast(commit.getNameOfChange.getKey)
    )
  }

  def asDraft(commit: Commit): Draft = {
    Draft(
      commit.getNameOfChange.getKey,
      simplePast(commit.getNameOfChange.getKey),
      commit.getTimestamp,
      asCode(commit.getSourceBeforeChange),
      asCode(commit.getSourceAfterChange)
    )
  }

  def asCommit(username:String, draft: Draft): Commit = {
    val before: Source      = asSource(draft.before)
    val after: Source       = asSource(draft.after)
    val cause: Name         = Refactoring.from(draft.cause)
    val description: String = draft.description

    val summary: CommitSummary = CommitSummary.forSuccessfulCommit(
      username,
      new Date(draft.timestamp),
      draft.cause + ": " + description
    )

    Commit.createValidCommit(
      cause,
      before,
      after,
      summary
    )
  }

  def asWarning(source: Source, each: Issue): Warning = {
    var marks:mutable.Buffer[Int]         = mutable.Buffer.empty[Int]
    val nodes:java.util.List[ASTNode]     = each.getAffectedNodes
    val nitr:java.util.Iterator[ASTNode]  = nodes.iterator()

    var names: mutable.Buffer[String] = mutable.Buffer.empty[String]
    while(nitr.hasNext){
      val node: ASTNode = nitr.next
      if(nodes.size() > 1){
        if(!node.isInstanceOf[MethodDeclaration]){
          names += getSimpleName(node)
          val loc: Location = Locations.locate(source, node)
          marks += loc.getStart.getOffset
          marks += loc.getEnd.getOffset
        }
      } else {
        val loc: Location = Locations.locate(source, node)
        marks += loc.getStart.getOffset
        marks += loc.getEnd.getOffset
        names += getSimpleName(node)
      }
    }

    Warning(
      names.mkString(","),
      each.getName.getKey,
      Some(marks.toList)
    )

  }

  private def getSimpleName(astNode: ASTNode): String = {
    astNode match {
      case method: MethodDeclaration =>
        method.getName.getIdentifier
      case simpleName: SimpleName =>
        simpleName.getIdentifier
      case declaration: VariableDeclaration =>
        declaration.getName.getIdentifier
      case _ => "UNKNOWN"
    }
  }


  private def simplePast(text: String): String = text match {
    case "Deduplicate"            => "Deduplicated code"
    case "Duplicate code"         => "Deduplicated code"
    case "Unformatted code"       => "Reformatted code"
    case "Reformat Code"          => "Reformatted code"
    case "Delete Type"            => "Removed unused type"
    case "Unused type"            => "Removed unused type"
    case "Delete Method"          => "Removed unused method"
    case "Unused method"          => "Removed unused method"
    case "Delete Parameter"       => "Removed unused parameter"
    case "Unused parameter"       => "Removed unused parameter"
    case "Delete Field"           => "Removed unused field"
    case "Unused field"           => "Removed unused field"
    case "Magic Number"           => "Removed unused magic number"
    case "Delete Region"          => "Removed code region"
    case "Delete Unused Imports"  => "Removed unused imports"
    case "Rename Method"          => "Renamed method"
    case "Rename Parameter"       => "Renamed parameter"
    case "Rename field"           => "Renamed field"
    case "Rename Type"            => "Renamed type"
    case "Rename variable"        => "Renamed local variable"
    case "Delete variable"        => "Removed local variable"
  }
}
