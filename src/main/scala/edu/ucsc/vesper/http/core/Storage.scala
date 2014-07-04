package edu.ucsc.vesper.http.core

import edu.ucsc.vesper.http.database.DatabaseSupport._
import edu.ucsc.vesper.http.domain.Models._
import reactivemongo.bson.BSONDocument

import scala.concurrent.{ExecutionContext, Future}

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
private[core] trait Storage {

  implicit def executionContext = ExecutionContext.Implicits.global

  def persist(code: Code): Future[Code] = Sources.add(code)

  def find(doc: BSONDocument): Future[Option[Result]] = Sources.find(doc).flatMap {
    codes => Future(Some(Result(sources = Some(codes))))
  }

  def findAll(): Future[Option[Result]] = find(BSONDocument.empty)

  def find(any: AnyInSet): Future[Option[Result]] = find(BSONDocument(any.name → BSONDocument("$in" → any.targets)))

  def find(exact: ExactlyOne): Future[Option[Result]] = find(BSONDocument(exact.name → exact.value))

  def find(exactlyAll: ExactlyAllInSet): Future[Option[Result]] = find(BSONDocument(exactlyAll.name → BSONDocument("$all" → exactlyAll.targets)))

  def findById(byId: ById): Future[Option[Result]] = Sources.findById(byId.value).flatMap {
    case Some(x) => Future(Some(Result(sources = Some(List(x)))))
    case None => Future(Some(Result(sources = Some(List()))))
  }

  def clear(): Future[Option[Result]] = Sources.removeAll().flatMap {
    lastError => Future(Some(Result(failure = Some(Failure(lastError.errMsg.get)))))
  }

}

case class VesperStorage() extends Storage
