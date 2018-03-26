package com.igeolise.chrome_headless_crawler.command_parser

object ActionParser {
  private val commandWithParam = "(\\w+) (.+)".r
  private val commandWithElement = "(\\w+) (\\S+)(?> (\\S+) (\\S+))?".r
  def unapply(command: String): Option[Action] = {
    command match {
      case commandWithParam("in", HtmlElementParser(element)) => Some(In(element))
      case commandWithParam("typeIn", text) => Some(TypeIn(text))
      case "click" => Some(Click)
      case "clickDownload" => Some(ClickDownload)
      case commandWithParam("navigateTo", url) => Some(NavigateTo(url))
      case commandWithElement("navigateToDownload", url, NullToOption(uName), NullToOption(pass)) =>
        val credentials = for (
          u <- uName;
          p <-pass
        ) yield Credentials(u, p)
        Some(NavigateToDownload(url, credentials))
      case "onCurrentPage" => Some(OnCurrentPage)
      case "up" => Some(Up)
      case commandWithParam("forAllElems", HtmlElementParser(elem)) => Some(ForAllElems(elem))
      case commandWithParam("findContainingInLastResult", text) =>
        Some(FindContainingInLastResult(text))
      case _ => None
    }
  }
}
