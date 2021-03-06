package com.vesperin.kiwi.domain

import spray.httpx.SprayJsonSupport
import spray.json.DefaultJsonProtocol

/**
 * @author hsanchez@cs.ucsc.edu (Huascar A. Sanchez)
 */
case class Warning(message: String)
case class Failure(message: String)
case class Info(messages: List[String])

case class Answer(items: List[String])
case class MultiStaged(stages: List[Stage])

// the result returned by Vesper; a refactoring or results request
case class Result(
      draft: Option[Draft]            = None,
      info: Option[Info]              = None,
      warnings: Option[List[Warning]] = None,
      failure: Option[Failure]        = None,
      sources: Option[List[Code]]     = None,
      stages: Option[MultiStaged]     = None,
      stage: Option[Stage]            = None
)


object Warning extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val failureFormats = jsonFormat1(Warning.apply)
}

object Failure extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val failureFormats = jsonFormat1(Failure.apply)
}

object Info extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val failureFormats = jsonFormat1(Info.apply)
}

object Answer extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val answerFormats = jsonFormat1(Answer.apply)
}

object MultiStaged extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val multiStagedFormats = jsonFormat1(MultiStaged.apply)
}

object Result extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val changeSummaryFormats = jsonFormat7(Result.apply)
}