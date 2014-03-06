package edu.ucsc.vesper.http.core

import akka.pattern.AskSupport
import akka.util.Timeout
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit._

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
trait AsyncSupport extends AskSupport {
  implicit val timeout: Timeout = Duration(5, SECONDS)
}
