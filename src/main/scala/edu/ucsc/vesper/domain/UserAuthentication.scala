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
   * Checks if a user has permissions to use vesper.
   * @param username The user's username.
   * @return <tt>true</tt> if user is authorized to use vesper.
   */
  def inTheClub(username: String): Boolean = whitelist.contains(username)
}
