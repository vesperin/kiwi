package vesperin.kiwi.spi

import vesperin.kiwi.domain.{Command, Find, Result}

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
trait Flattener {

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
      command.persist,
      command.update,
      command.preprocess,
      command.slice,
      command.multistage,
      command.summarize
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
      result.sources,
      result.stages,
      result.stage
    ).flatten

    if (answer == Nil) Nil else answer(0)
  }

}

case class ResultFlattener() extends Flattener
