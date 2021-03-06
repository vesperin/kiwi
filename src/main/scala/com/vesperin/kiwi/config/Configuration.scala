package com.vesperin.kiwi.config

import com.typesafe.config.{ConfigValue, ConfigFactory}
import java.util
import java.util.Map.Entry
import scala.collection.mutable
import scala.util.{Properties, Try}

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

  val mongoHQRegEx  = """mongodb://(\w+):([\w|-]+)@([\w|\.]+):(\d+)/(\w+)""".r

  lazy val remoteDatabase: String = Properties.envOrNone("MONGOHQ_URL") match {
    case Some(x) => x match {
      case mongoHQRegEx(user, password, host, port, databaseName) => databaseName
      case _ => dbName
    }

    case None    => dbName
  }

  /** User name used to access database. */
  lazy val dbUser = Try(config.getString("db.user")).toOption.orNull

  /** Password for specified user and database. */
  lazy val dbPassword = Try(config.getString("db.password")).toOption.orNull

  // twitter info

  lazy val OAuthConsumerKey       = Try(config.getString("twitter.OAuthConsumerKey")).toOption.orNull
  lazy val OAuthConsumerSecret    = Try(config.getString("twitter.OAuthConsumerSecret")).toOption.orNull
  lazy val OAuthAccessToken       = Try(config.getString("twitter.OAuthAccessToken")).toOption.orNull
  lazy val OAuthAccessTokenSecret = Try(config.getString("twitter.OAuthAccessTokenSecret")).toOption.orNull
}