package edu.ucsc.vesper.http.core

import edu.ucsc.vesper.http.domain.Models._

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
trait Parser {
  private def tokenize(s: String, p:String) = p.r.split(s).toList

  private def tokenize(q: String): List[String] = tokenize(
    q,
    """:"""
  )

  private def massageArgs(args:List[String]): List[String] = tokenize(
    args.mkString(" "),
    """\s"""
  )

  private def parse(command: String, args: List[String]): Command =
    command match {
      case "all"     => Command(find = Some(Find(all    = Some(AllInSet()))))
      case "name"    => Command(find = Some(Find(exact  = Some(ExactlyOne(command, args(0))))))
      case "tags"    => Command(find = Some(Find(any    = Some(AnyInSet(command, args)))))
      case "algs"    => Command(find = Some(Find(any    = Some(AnyInSet("algorithms", args)))))
      case "ds"      => Command(find = Some(Find(any    = Some(AnyInSet("datastructures", args)))))
      case "url"     => Command(find = Some(Find(exact  = Some(ExactlyOne(command, args(0))))))
      case "id"      => Command(find = Some(Find(byId   = Some(ById(args(0))))))
      case "roles"   => Command(find = Some(Find(roles  = Some(ExactRole(args(0))))))
      case `command` => Command()
    }

  private def parse(args: List[String]): Command = {
    args match {
      case Nil             => Command() // Nil or nothing
      case command :: rest => parse(command, massageArgs(rest))
    }
  }

  def parse(s: String): Command = parse(tokenize(s))
}


case class CommandParser() extends Parser