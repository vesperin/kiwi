package edu.ucsc.vesper.config

import com.typesafe.config.{ConfigValue, ConfigFactory}
import scala.util.Try
import java.util
import java.util.Map.Entry
import scala.collection.mutable

/**
 * Holds service configuration settings.
 *
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
trait ServiceConfiguration {
  /**
   * Application config object.
   */
  val config = ConfigFactory.load()

  /** Database host name/address. */
  lazy val dbHost     = Try(config.getString("db.host")).getOrElse("localhost")

  /** Database host port number. */
  lazy val dbPort     = Try(config.getInt("db.port")).getOrElse(3306)

  /** Service database name. */
  lazy val dbName     = Try(config.getString("db.name")).getOrElse("vesper")

  /** User name used to access database. */
  lazy val dbUser     = Try(config.getString("db.user")).toOption.orNull

  /** Password for specified user and database. */
  lazy val dbPassword = Try(config.getString("db.password")).toOption.orNull

  lazy val users = config.getConfig("users")
  val itr:util.Iterator[Entry[String, ConfigValue]] = users.entrySet().iterator()

  /** a white list holding plain-text username entries **/
  // todo(Huascar) this whitelist can be a Map where the key is the username and the value
  // a set of scenarios: curating session or replaying history session
  lazy val whitelist = mutable.Set.empty[String]

  while(itr.hasNext){
    val  x: Entry[String, ConfigValue] = itr.next()
    whitelist += x.getKey
  }



}
