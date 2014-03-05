package edu.ucsc.vesper.http.core

import edu.ucsc.refactor.{Vesper, Refactorer}

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
trait Curator {
  val refactorer: Refactorer = Vesper.createRefactorer()
  // todo(Huascar) to implement
}
