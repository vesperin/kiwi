package vesperin.kiwi.db

import vesperin.kiwi.config.Configuration
import vesperin.kiwi.domain.Code
import reactivemongo.api.{MongoConnection, MongoDriver}
import reactivemongo.bson.BSONDocument
import spray.json.RootJsonFormat
import sprest.Formats._
import sprest.models.UniqueSelector
import sprest.reactivemongo.ReactiveMongoPersistence
import sprest.reactivemongo.typemappers._

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Properties

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
trait ReactiveMongoStorage extends ReactiveMongoPersistence with Configuration {
  object ConnectionMaker extends Configuration {
    def unapply(url: Option[String]): Option[MongoConnection] = {
      val driver = new MongoDriver

      url match {
        case Some(mongoHQRegEx(user, password, host, port, databaseName)) =>
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
  // Gets a reference to the database 'sources'
  println("The database is " + remoteDatabase + " \n")
  val db = connection(remoteDatabase)


  // Json mapping to / from BSON - in this case we want "_id" from BSON to be
  // mapped to "id" in JSON in all cases
  implicit object JsonTypeMapper extends SprayJsonTypeMapper with NormalizedIdTransformer

  abstract class UnsecuredDAO[M <: sprest.models.Model[String]](
     collName: String)(withNewId: M => M)(
     implicit
     jsformat: RootJsonFormat[M]) extends CollectionDAO[M, String](db(collName)) {

    case class Selector(id: String) extends UniqueSelector[M, String]

    override def generateSelector(id: String) = Selector(id)
    override protected def addImpl(m: M)(implicit ec: ExecutionContext)    = doAdd(withNewId(m))
    override protected def updateImpl(m: M)(implicit ec: ExecutionContext) = doUpdate(m)
    override def remove(selector: Selector)(implicit ec: ExecutionContext) = uncheckedRemoveById(selector.id)

    def database    = db

    def removeAll()(implicit ec: ExecutionContext) = collection.remove(BSONDocument.empty)
  }

  def newGuid = java.util.UUID.randomUUID.toString

  // MongoDB collections:
  object Sources extends UnsecuredDAO[Code]("sources")(_.copy(id = newGuid))
}

object ReactiveMongoStorage extends ReactiveMongoStorage
