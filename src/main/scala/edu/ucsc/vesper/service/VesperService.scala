package edu.ucsc.vesper.service

import spray.routing._
import akka.actor._
import akka.routing.RoundRobinRouter
import edu.ucsc.vesper.domain.Command
import akka.pattern.AskSupport
import akka.util.Timeout
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit._


// we don't implement our route structure directly in the service actor because
// we want to be able to test it independently, without having to spin up an actor
class VesperActor extends Actor with VesperService {
  // the HttpService trait defines only one abstract member, which
  // connects the services environment to the enclosing actor or test
  override def actorRefFactory = context

  // this actor only runs our route, but you could add
  // other things here, like request stream processing
  // or timeout handling
  override def receive = runRoute(vesperRoutes)
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
trait VesperService extends HttpService with AsyncSupport {

  def backend = actorRefFactory.actorOf(
    Props[VesperinActor].withRouter(
      RoundRobinRouter(
        nrOfInstances = 1000
      )
    )
  )

  val vesperRoutes =
    path("") {
      get {
        parameter('q) {
          q =>
            complete(
              s"The query is '$q'"
            )
        }
      } ~
        (put | post | parameter('method ! "c")) {
          entity(as[Command]) {
            command =>
              complete(command)
          }
        }
    }
}  	