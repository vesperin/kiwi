package edu.ucsc.vesper.http.config

import com.typesafe.config.{ConfigValue, ConfigFactory}
import scala.collection.mutable
import java.util
import java.util.Map.Entry

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
}
