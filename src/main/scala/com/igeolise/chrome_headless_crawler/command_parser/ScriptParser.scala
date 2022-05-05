package com.igeolise.chrome_headless_crawler.command_parser

import cats.syntax.traverse._
import cats.instances.{either, vector}
import either._, vector._

object ScriptParser {

  case class ScriptParsingError(msg: String)

  def parse(script: String): Either[ScriptParsingError, Vector[Action]] = {
    if (script.isEmpty) {
      Left(ScriptParsingError("Script is empty"))
    } else {
      val parsedParts = script.split("\\\\").map {
        case ActionParser(action) => Right(action)
        case s: String => Left(ScriptParsingError(s"$s is not a valid action"))
      }.toVector
      parsedParts.sequence
    }
  }
}

