package edu.ucsc.vesper.http

import akka.actor.{Props, ActorSystem}
import edu.ucsc.vesper.http.api.VesperinApi
import scala.util.Properties
import akka.util.Timeout
import akka.io.IO
import spray.can.Http
import scala.concurrent.duration._

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
class Main extends App {
  // The ActorSystem to host our application in
  implicit val system = ActorSystem("on-vesper-http")

  // create and start our service actor
  val service = system.actorOf(Props[VesperinApi], "vesper-http")

  val port = Properties.envOrElse("PORT", "8080").toInt // for Heroku compatibility

  implicit val timeout = Timeout(5.seconds)

  // start a new HTTP server on port 8080 with our service actor as the handler
  IO(Http) ! Http.Bind(service, "0.0.0.0", port)
}
