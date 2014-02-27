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

  lazy val permissions = config.getConfig("permissions")
  val itr:util.Iterator[Entry[String, ConfigValue]] = permissions.entrySet().iterator()

  /** a white list holding plain-text username/permission entries **/
  lazy val whitelist = mutable.Map.empty[String, String]

  while(itr.hasNext){
    val  x: Entry[String, ConfigValue] = itr.next()
    whitelist(x.getKey) = x.getValue.render
  }



}
