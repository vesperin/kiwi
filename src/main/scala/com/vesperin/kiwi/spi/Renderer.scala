package com.vesperin.kiwi.spi

import com.vesperin.kiwi.database.{MongoStorage, Storage}
import com.vesperin.kiwi.domain.{ExactlyOne, Code, Result}
import com.vesperin.kiwi.util.Html

import scala.concurrent.{ExecutionContextExecutor, Future, ExecutionContext}
import scala.xml.Unparsed

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
trait Renderer {

  implicit def executionContext: ExecutionContextExecutor = ExecutionContext.Implicits.global
  
  val flattener: Flattener = ResultFlattener()


  private[this] def flatResult(optionResult: Option[Result]): Future[List[Code]] = optionResult match {
    case Some(r)  =>

      Future {
        val answer: List[Code] = flattener.flatten(r) match {
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
        Html.buildCodesnippetHtml(
          c, 
          relatedWork,
          survey
        )
      )
    case None    => 
      Future(
        Html.buildErrorPage()
      )
  }


  private[this] def buildIndexHtml(): Future[Unparsed] = Future(Html.buildStatusPage())

  private[this] def buildErrorHtml(): Future[Unparsed] = Future(Html.buildErrorPage())


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
      storage     <- Future(MongoStorage())
      relatedWork <- collectRelatedWork(theCode, storage)
      unparsed    <- buildHtml(theCode, relatedWork, survey)
    } yield {
      unparsed
    }
  }

  def renderStatusHtml(): Future[Unparsed] = {
    for{
      page <- buildIndexHtml()
    } yield {
      page
    }
  }

  def renderError(): Future[Unparsed] = {
    for{
      page <- buildErrorHtml()
    } yield {
      page
    }
  }

}

case class HtmlRenderer() extends Renderer
