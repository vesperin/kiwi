package edu.ucsc.vesper.domain


import edu.ucsc.vesper.config.ServiceConfiguration
import spray.routing.authentication.UserPass


/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
trait UserAuthentication extends ServiceConfiguration {
  /**
   * extract the username from a UserPass object.
   * @param userpass The UserPass object
   * @return the username
   */
  def getUsername(userpass: UserPass): String = userpass.user

  /**
   * Checks whether the user is allow to read a resource
   * @param username The user to be checked
   * @return true if allowed, false otherwise.
   */
  def isAllowReading(username: String): Boolean = inTheClub(username, "r")

  /**
   * Checks whether the user is allow to write a resource
   * @param username The user to be checked
   * @return true if allowed, false otherwise.
   */
  def isAllowWriting(username: String): Boolean = inTheClub(username, "w")


  /**
   * Checks if a user has permissions to use vesper.
   * @param username The user's username.
   * @param permission The user's permission
   * @return <tt>true</tt> if user is authorized to use vesper.
   */
  private def inTheClub(username: String, permission:String): Boolean = if (whitelist.contains(username))
    whitelist(username).contains(permission) else
    false
}
