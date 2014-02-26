package edu.ucsc.vesper.service

import akka.actor.Actor
import akka.event.Logging
import edu.ucsc.vesper.config.ServiceConfiguration
import java.sql._
import scala.slick.driver.MySQLDriver.simple.Database.threadLocalSession
import scala.slick.driver.MySQLDriver.simple._
import slick.jdbc.meta.MTable
import edu.ucsc.vesper.domain.{SearchParameters, Curating}
import edu.ucsc.vesper.database.Curatings


/**
 * Message object for a request to load all previous sessions
 * started by user matching a given username and password.
 *
 * the use case scenario is when u have several curating sessions.
 * The Web browser extension settings will show the user (once this info
 * is provided) a list of your sessions
 */
case class All(username:String, password:String)

/**
 * Message object for a request to create (or start) a new curating session.
 */
case class Create(session: Curating)

/**
 * Message object for a request to delete a previous curating session
 * identified by a given id
 */
case class Delete(id: Long)

/**
 * Message object for a request to load a previous session
 * identified by a given id
 */
case class Get(id:Long)

/**
 * Message object for a request to modify a previous curating session.
 * The item to modify is passed along with the message object.
 */
case class Update(id:Long, session: Curating)


/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
trait Vesperin extends ServiceConfiguration {
  // init Database instance
  private val db = Database.forURL(
    url  = "jdbc:mysql://%s:%d/%s".format(dbHost, dbPort, dbName),
    user = dbUser, password = dbPassword, driver = "com.mysql.jdbc.Driver"
  )

  // create tables if not exist
  db.withSession {
    if(MTable.getTables("curating").list().isEmpty){
      Curatings.ddl.createStatements
    }
  }

  def create(session: Curating): Option[Curating] = {
    try {
      val id = db.withSession {
        Curatings returning Curatings.id insert session
      }

      Some(session.copy(id = Some(id)))
    } catch {
      case e: SQLException => None
    }
  }

  /**
   * Retrieves specific curating session from database.
   *
   * @param id id of the curating session to retrieve
   * @return an Option, None iff the Curating wasn't found in the database
   */
  def get(id: Long): Option[Curating] = {
    try {
      db.withSession {
        Curatings.findById(id).firstOption
      }
    } catch {
      case e: SQLException => None
    }
  }

  /**
   * Retrieves All specific curating session from database.
   *
   * @param username username of curator
   * @param password password of curator
   * @return List of curating entities started by user, empty list iff
   *         the user information is not found in the database.
   */
  def all(username:String, password:String): List[Curating] = {
    try {
      db.withSession {
        search(
          SearchParameters(
            None,
            None,
            Some(username),
            Some(password),
            None,
            None
          )
        )
      }
    } catch {
      case e: SQLException =>
        println(e.getMessage)
        List()
    }
  }

  /**
   * Retrieves list of curating sessions with specified parameters from database.
   *
   * @param params search parameters
   * @return list of curating sessions that match given parameters
   */
  def search(params: SearchParameters): List[Curating] = {
    implicit val typeMapper = Curatings.dateTypeMapper

    try {
      db.withSession {
        val query = for {
          curating <- Curatings if {
          Seq(
            params.id.map(curating.id is _),
            params.sessionname.map(curating.sessionname is _),
            params.username.map(curating.username is _),
            params.password.map(curating.password is _),
            params.gistid.map(curating.gistid is _),
            params.birthday.map(curating.birthday is _)
          ).flatten match {
            case Nil => ConstColumn.TRUE
            case seq => seq.reduce(_ && _)
          }
        }
        } yield curating

        query.run.toList
      }
    }

  }

  /**
   * Deletes curating session from database.
   *
   * @param id id of the curating session to delete
   * @return An Option containing the deleted curating entity, None
   *         iff nothing was deleted.
   */
  def delete(id: Long): Option[Curating] = {
    try {
      db.withTransaction {
        val query     = Curatings.where(_.id === id)
        val curatings = query.run.asInstanceOf[List[Curating]]
        curatings.size match {
          case 0 => None
          case _ =>
            query.delete
            Some(curatings.head)
        }
      }
    } catch {
      case e: SQLException =>
        println(e.getMessage)
        None
    }
  }


  /**
   * Updates curating session entity with specified one.
   *
   * @param id       id of the curating session to update.
   * @param customer updated curating session entity
   * @return updated curating session entity
   */
  def update(id: Long, customer: Curating): Option[Curating] = {
    try
      db.withSession {
        Curatings.where(_.id === id) update customer.copy(id = Some(id)) match {
          case 0 => None
          case _ => Some(customer.copy(id = Some(id)))
        }
      }
    catch {
      case e: SQLException =>
        println(e.getMessage)
        None
    }
  }

}

class VesperinActor extends Actor with Vesperin {
  val log = Logging(context.system, this)

  override def receive = {
    case Get(id) =>
      log.info(id.toString)
      val curating = get(id)
      curating match {
        case None     => log.info("nothing found")
        case Some(x)  => log.info(x.sessionname)
      }

      sender ! curating

    case Update(id, session) => sender ! update(id, session)

    case Delete(id) =>
      log.info("delete called")
      sender ! delete(id)

    case Create(session) => sender ! create(session)

    case All(username: String, password: String) => sender ! all(username, password)
  }
}





