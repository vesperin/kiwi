package edu.ucsc.vesper.http.database

import reactivemongo.api.{MongoDriver, MongoConnection}
import scala.util.Properties
import sprest.models.{UUIDStringId, UniqueSelector}
import scala.concurrent.ExecutionContext
import reactivemongo.bson.BSONDocument

import sprest.reactivemongo.ReactiveMongoPersistence
import sprest.reactivemongo.typemappers._
import sprest.Formats._
import spray.json.RootJsonFormat
import edu.ucsc.vesper.http.config.Configuration
import edu.ucsc.vesper.http.domain.Models.Code
import scala.concurrent.ExecutionContext.Implicits.global


/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 *
 */
trait DatabaseSupport extends ReactiveMongoPersistence {
  object ConnectionMaker extends Configuration {
    def unapply(url: Option[String]): Option[MongoConnection] = {
      val regex  = """mongodb://(\w+):(\w+)@([\w|\.]+):(\d+)/(\w+)""".r
      val driver = new MongoDriver

      url match {
        case Some(regex(user, password, host, port, databaseName)) =>
          val connection     = driver.connection(List("%s:%s".format(host, port)))
          connection.authenticate(databaseName, user, password)
          Some(connection)
        case None =>
          val localConnection = driver.connection(List("%s:%s".format(dbHost, dbPort)))
          localConnection.authenticate(dbName, dbUser, dbPassword)
          Some(localConnection)
      }
    }
  }

  val ConnectionMaker(connection) = Properties.envOrNone("MONGOHQ_URL")
  // Gets a reference to the database 'codesnippets'
  val db =  connection("vesper")


  // Json mapping to / from BSON - in this case we want "_id" from BSON to be
  // mapped to "id" in JSON in all cases
  implicit object JsonTypeMapper extends SprayJsonTypeMapper with NormalizedIdTransformer

  abstract class UnsecuredDAO[M <: sprest.models.Model[String]](collName: String)(implicit jsformat: RootJsonFormat[M])
    extends CollectionDAO[M, String](db(collName)) {

    case class Selector(id: String) extends UniqueSelector[M, String]

    override def generateSelector(id: String) = Selector(id)
    override protected def addImpl(m: M)(implicit ec: ExecutionContext) = doAdd(m)
    override protected def updateImpl(m: M)(implicit ec: ExecutionContext) = doUpdate(m)
    override def remove(selector: Selector)(implicit ec: ExecutionContext) = uncheckedRemoveById(selector.id)


    def database    = db

    def removeAll()(implicit ec: ExecutionContext) = collection.remove(BSONDocument.empty)
  }

  // MongoDB collections:
  object Sources extends UnsecuredDAO[Code]("sources") with UUIDStringId
}

object DatabaseSupport extends DatabaseSupport
