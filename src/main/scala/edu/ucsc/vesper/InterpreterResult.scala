package edu.ucsc.vesper


/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
object InterpreterResult {
  abstract sealed class Result

  case object Success extends Result
  case object Error extends Result
  case object Incomplete extends Result


}
