package com.igeolise.chrome_headless_crawler.command_parser

import scalaz.{-\/, \/, \/-}
import scalaz.syntax.traverse._
import scalaz.std.vector._

object ScriptParser {

  case class ScriptParsingError(msg: String)

  def parse(script: String): \/[ScriptParsingError, Vector[Action]] = {
    if (script.isEmpty) {
      -\/(ScriptParsingError("Script is empty"))
    } else {
      script.split("\\\\").map {
        case ActionParser(action) => \/-(action)
        case s: String => -\/(ScriptParsingError(s"$s is not a valid action"))
      }.toVector.sequenceU
    }
  }
}

