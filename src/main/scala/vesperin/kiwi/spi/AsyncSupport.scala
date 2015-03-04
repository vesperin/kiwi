package vesperin.kiwi.spi

import java.util.concurrent.TimeUnit._

import akka.pattern.AskSupport
import akka.util.Timeout

import scala.concurrent.duration.Duration

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
trait AsyncSupport extends AskSupport {
  implicit val timeout: Timeout = Duration(2, SECONDS)
}
