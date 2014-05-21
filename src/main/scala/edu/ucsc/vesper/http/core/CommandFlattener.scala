package edu.ucsc.vesper.http.core

import edu.ucsc.vesper.http.domain.Models.Command

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
trait CommandFlattener {

  def flatten(command: Command) = {
    val answer = List(
      command.inspect,
      command.remove,
      command.rename,
      command.optimize,
      command.format,
      command.deduplicate,
      command.cleanup,
      command.publish,
      command.persist
    ).flatten

    answer(0)
  }

}
