package vesperin.kiwi

import akka.actor.{ActorSystem, Props}
import akka.io.IO
import akka.routing.RoundRobinPool
import akka.util.Timeout
import spray.can.Http
import vesperin.kiwi.routes.KiwiActor

import scala.concurrent.duration._
import scala.util.Properties

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
object Boot extends App {

   // we need an ActorSystem to host our application in
   implicit val system = ActorSystem("on-kiwi")

   // create and start our service actor
   val service = system.actorOf(
     RoundRobinPool(20).props(Props[KiwiActor]),
     "kiwi"
   )

   // for Heroku compatibility
   val port = Properties.envOrElse("PORT", "8080").toInt

   implicit val timeout = Timeout(5.seconds)

   // start a new HTTP server on port 8080 with our service actor as the handler
   IO(Http) ! Http.Bind(service, "0.0.0.0", port)
 }
