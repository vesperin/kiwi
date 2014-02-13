package edu.ucsc.vesper

import spray.routing._
import akka.actor._
import edu.ucsc.refactor.cli.{Parser, Result, Interpreter}
import edu.ucsc.refactor.{Refactorer, Source, Vesper}

// we don't implement our route structure directly in the service actor because
// we want to be able to test it independently, without having to spin up an actor
class VesperActor extends Actor with VesperService {
  // the HttpService trait defines only one abstract member, which
  // connects the services environment to the enclosing actor or test
  def actorRefFactory = context

  // this actor only runs our route, but you could add
  // other things here, like request stream processing
  // or timeout handling
  def receive = runRoute(vesperRoutes)
}

// this trait defines vesper behavior independently from the vesper actor	
trait VesperService extends HttpService {

  val interpreter:Interpreter = new Interpreter
  val parser:Parser           = new Parser
  val result:Result           = interpreter.eval(parser.parse("help"))

  val vesperRoutes =
    path("") {
      get {
        parameter('q) {q =>
            complete(s"The query is '$q'")
        }
      } ~
      (put | post) {
        entity(as[Command]) {
          command =>
            complete(command)
        }
      }
    }
}  	