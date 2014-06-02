package edu.ucsc.vesper.http.config

import com.typesafe.config.{ConfigValue, ConfigFactory}
import java.util
import java.util.Map.Entry
import scala.collection.mutable
import scala.util.Try

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
trait Configuration {
  /**
   * Application config object.
   */
  val config = ConfigFactory.load()

  /** a list of user entries **/
  lazy val users = config.getConfig("users")
  /** a username->password map **/
  lazy val passwords = mutable.Map.empty[String, String]
  val userIterator:util.Iterator[Entry[String, ConfigValue]] = users.entrySet().iterator()
  while(userIterator.hasNext){
    val  x: Entry[String, ConfigValue] = userIterator.next()
    passwords(x.getKey) = x.getValue.render
  }

  /** a list of role entries **/
  lazy val roles =  config.getConfig("roles")
  /** a username->role map **/
  lazy val club = mutable.Map.empty[String, Int]
  val roleIterator:util.Iterator[Entry[String, ConfigValue]] = roles.entrySet().iterator()
  while(roleIterator.hasNext){
    val  x: Entry[String, ConfigValue] = roleIterator.next()
    club(x.getKey) = x.getValue.render.toInt
  }


  /** Database host name/address. */
  lazy val dbHost = Try(config.getString("db.host")).getOrElse("localhost")

  /** Database host port number. */
  lazy val dbPort = Try(config.getInt("db.port")).getOrElse(27017)

  /** Service database name. */
  lazy val dbName = Try(config.getString("db.name")).getOrElse("vesper")

  /** User name used to access database. */
  lazy val dbUser = Try(config.getString("db.user")).toOption.orNull

  /** Password for specified user and database. */
  lazy val dbPassword = Try(config.getString("db.password")).toOption.orNull
}