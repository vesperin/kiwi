package edu.ucsc.vesper.http.core

import edu.ucsc.vesper.http.domain.Models.{Result, Find, Command}

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
      command.find,
      command.persist
    ).flatten

    if (answer == Nil) Nil else answer(0)
  }

  def flatten(find: Find) = {
    val answer = List(
      find.all,
      find.any,
      find.exact,
      find.exactlyAll,
      find.byId,
      find.roles
    ).flatten

    if (answer == Nil) Nil else answer(0)
  }

  def flatten(result: Result) = {
    val answer = List(
      result.draft,
      result.info,
      result.warnings,
      result.failure,
      result.sources
    ).flatten

    if (answer == Nil) Nil else answer(0)
  }

}
