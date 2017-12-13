package crawler.command_parser

object ScriptParser {
  class ParsingException(msg: String) extends Exception(msg)
  def parse(script: String): Either[Exception, Seq[Action]] = {
    if (script.isEmpty) {
      Left(new ParsingException("Script is empty"))
    } else try {
      Right(script.split("\\\\").map {
        case ActionParser(action) =>
          action
        case s: String => throw new ParsingException(s"$s is not a valid action")
      }.toSeq)
    } catch { case e: ParsingException => Left(e)  }
  }
}

