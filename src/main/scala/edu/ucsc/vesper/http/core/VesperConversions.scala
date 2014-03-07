package edu.ucsc.vesper.http.core

import edu.ucsc.refactor.{Location, Issue, Note, Source}
import edu.ucsc.vesper.http.domain.LoungeObjects.{Warning, Draft, Comment, Code}
import scala.collection.mutable
import edu.ucsc.refactor.util.{Locations, Commit}
import edu.ucsc.refactor.spi.{CommitSummary, Refactoring, Name}
import java.util.Date
import org.eclipse.jdt.core.dom.{MethodDeclaration, ASTNode}

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
trait VesperConversions {
  def asCode(source: Source): Code = {
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

    Code(
      id          = sourceId,
      name        = sourceName,
      description = sourceDesc,
      content     = sourceCont,
      comments    = srcComments
    )
  }

  def asSource(source: Code): Source = {
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

  def asDraft(commit: Commit): Draft = {
    Draft(
      commit.getNameOfChange.getKey,
      commit.getNameOfChange.getSummary,
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


    while(nitr.hasNext){
      val node: ASTNode = nitr.next
      if(nodes.size() > 1){
        if(!node.isInstanceOf[MethodDeclaration]){
          val loc: Location = Locations.locate(source, node)
          marks += loc.getStart.getOffset
          marks += loc.getEnd.getOffset
        }
      } else {
        val loc: Location = Locations.locate(source, node)
        marks += loc.getStart.getOffset
        marks += loc.getEnd.getOffset
      }
    }

    Warning(
      each.getName.getKey,
      each.getName.getSummary,
      Some(marks.toList)
    )

  }
}
