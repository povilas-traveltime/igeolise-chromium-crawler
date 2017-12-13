package crawler.command_parser

object ActionParser {
  private val regex = "(\\w+) (.+)".r
  private val regexThree = "(\\w+) (\\S+) (.+)".r
  private val regexFour = "(\\w+) (\\S+)(?> (\\S+) (\\S+))?".r
  def unapply(command: String): Option[Action] = {
    command match {
      case regex("in", HtmlElementParser(element)) => Some(In(element))
//      case regex("from", HtmlElementParser(element)) => Some(From(element))
      case regex("typeIn", text) => Some(TypeIn(text))
      case "click" => Some(Click)
//      case "clickDownload" => Some(ClickDownload)
//      case "clickGetResult" => Some(ClickGetResult)
//      case "mouseOver" => Some(MouseOver)
      case regex("navigateTo", url) => Some(NavigateTo(url))
      case regexFour("navigateToDownload", url, NullToOption(uName), NullToOption(pass)) =>
        val credentials = for (
          u <- uName;
          p <-pass
        ) yield (u, p)
        Some(NavigateToDownload(url, credentials))
      case "onCurrentPage" => Some(OnCurrentPage)
      case "up" => Some(Up)
      case regex("forAllElems", HtmlElementParser(elem)) => Some(ForAllElems(elem))
      case regex("findContainingInLastResult", text) =>
        Some(FindContainingInLastResult(text))
      case _ => None
    }
  }
}
