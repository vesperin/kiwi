package edu.ucsc.vesper.service

import spray.routing._
import akka.actor._
import akka.routing.RoundRobinRouter
import edu.ucsc.vesper.domain.{UserAuthentication, Command}
import akka.pattern.AskSupport
import akka.util.Timeout
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit._
import scala.concurrent.ExecutionContext.Implicits.global
import spray.routing.authentication.BasicAuth


// we don't implement our route structure directly in the service actor because
// we want to be able to test it independently, without having to spin up an actor
class VesperActor extends Actor with VesperService {
  // the HttpService trait defines only one abstract member, which
  // connects the services environment to the enclosing actor or test
  override def actorRefFactory = context

  // this actor only runs our route, but you could add
  // other things here, like request stream processing
  // or timeout handling
  override def receive = runRoute(vesperRoute)
}

/**
 * Configuration of Akkas asynchronous AskSupport
 *
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
trait AsyncSupport extends AskSupport {
  implicit val timeout: Timeout = Duration(5, SECONDS)
}

/**
 * Defines vesper behavior independently from the vesper actor
 *
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
trait VesperService extends HttpService with AsyncSupport with UserAuthentication {

  def backend = actorRefFactory.actorOf(
    Props[VesperinActor].withRouter(
      RoundRobinRouter(
        nrOfInstances = 1000
      )
    )
  )

  val vesperRoute =
    authenticate(BasicAuth(realm = "secure site", users, getUsername _)) { userName =>
      path("") {
        get {
          authorize(inTheClub(userName)) {
            parameter('q) {
              q =>
                complete(s"The query is '$q'")
            }

          }
        } ~
          (put | post | parameter('method ! "c")) {
            authorize(inTheClub(userName)) {
              entity(as[Command]) {
                command =>
                  complete(command)
              }
            }
          }
      }
    }
}  	