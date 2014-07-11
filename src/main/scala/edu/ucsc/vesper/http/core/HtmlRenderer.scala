package edu.ucsc.vesper.http.core

import edu.ucsc.vesper.http.domain.Models.{ExactlyOne, Code, Result}
import edu.ucsc.vesper.http.util.Html

import scala.concurrent.{Future, ExecutionContext}
import scala.xml.Unparsed

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
trait HtmlRenderer extends Flattener {

  implicit def executionContext = ExecutionContext.Implicits.global


  private[this] def flatResult(optionResult: Option[Result]): Future[List[Code]] = optionResult match {
    case Some(r)  =>

      Future {
        val answer: List[Code] = flatten(r) match {
          case Nil    => List()
          case x::xs  => (x :: xs).asInstanceOf[List[Code]]
        }

        answer
      }

    case None    => Future (List())

  }


  private[this] def collectRelatedWork(theCode: Option[Code], storage: Storage): Future[List[Code]] = theCode match {
    case Some(c) =>

      for {
        result        <- storage.find(ExactlyOne("url", c.url.getOrElse("http://www.chefedited.com")))
        relatedWork   <- flatResult(result)
      } yield {
        relatedWork
      }

    case None    =>  Future(List())
  }


  private[this] def findSingleCode(list: List[Code]): Future[Option[Code]] = {
    Future {
      list match {
        case x :: xs  => Some(x)
        case _        => None
      }
    }
  }


  private[this] def buildHtml(theCode: Option[Code], relatedWork: List[Code], survey: Boolean): Future[Unparsed] = theCode match {
    case Some(c) => 
      Future(
        Html.renderCodesnippet(
          c, 
          relatedWork,
          survey
        )
      )
    case None    => 
      Future(
        Html.ohSnap()
      )
  }


  private[this] def collectCode(res: Option[Result]): Future[Option[Code]] = {
    for {
      codeList <- flatResult(res)
      theCode  <- findSingleCode(codeList)
    } yield {
      theCode
    }
  }

   def renderHtml(optionResult: Option[Result], survey: Boolean): Future [Unparsed] = {
    for {
      theCode     <- collectCode(optionResult)
      storage     <- Future(VesperStorage())
      relatedWork <- collectRelatedWork(theCode, storage)
      unparsed    <- buildHtml(theCode, relatedWork, survey)
    } yield {
      unparsed
    }
  }

}

case class CodeHtmlRenderer() extends HtmlRenderer
