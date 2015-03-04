package vesperin.kiwi.routes

import akka.actor.Actor

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
class KiwiActor extends Actor with Kiwi {

  // the HttpService trait defines only one abstract member, which
  // connects the services environment to the enclosing actor or test
  def actorRefFactory = context

  // this actor only runs our route, but you could add
  // other things here, like request stream processing
  // or timeout handling
  def receive = runRoute(routes)
}
