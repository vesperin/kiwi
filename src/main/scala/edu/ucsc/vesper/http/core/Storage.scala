package edu.ucsc.vesper.http.core

import edu.ucsc.vesper.http.domain.Models._
import edu.ucsc.vesper.http.database.DatabaseSupport._
import scala.concurrent.{ExecutionContext, Future}
import reactivemongo.bson.BSONDocument
import scala.Some

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
private[core] trait Storage {

  implicit def executionContext = ExecutionContext.Implicits.global

  def persist(code: Code): Future[Option[Result]] = Sources.add(code).flatMap {
    theCode => Future(Some(Result(info = Some(Info(List("%s was saved!".format(theCode.name)))))))
  }

  def find(doc: BSONDocument): Future[Option[Result]] = Sources.find(doc).flatMap {
    codes => Future(Some(Result(sources = Some(codes))))
  }

  def findAll(): Future[Option[Result]] = find(BSONDocument.empty)

  def find(any: Any): Future[Option[Result]] = find(BSONDocument(any.name → BSONDocument("$in" → any.targets)))

  def find(exact: Exact): Future[Option[Result]] = find(BSONDocument(exact.name → exact.value))

  def find(exactlyAll: ExactlyAll): Future[Option[Result]] = find(BSONDocument(exactlyAll.name → BSONDocument("$all" → exactlyAll.targets)))

  def clear(): Future[Option[Result]] = Sources.removeAll().flatMap {
    lastError => Future(Some(Result(failure = Some(Failure(lastError.errMsg.get)))))
  }

}

case class VesperStorage() extends Storage
