package com.igeolise.chrome_headless_crawler.command_parser

object ActionParser {
  private val commandWithParam = "(\\w+) (.+)".r
  private val commandWithNumber = "(\\w+) (\\d+)".r
  private val commandWithElement = "(\\w+) (\\S+)(?> (\\S+) (\\S+))?".r
  private val commandWithElementAndSubstring = "(\\w+) (.+) (.+)".r
  def unapply(command: String): Option[Action] = {
    command match {
      case commandWithParam("in", HtmlElementParser(element)) => Some(In(element))
      case commandWithParam("typeIn", text) => Some(TypeIn(text))
      case "click" => Some(Click)
      case "clickNoWait" => Some(ClickNoWait)
      case "clickDownload" => Some(ClickDownload)
      case commandWithParam("navigateTo", url) => Some(NavigateTo(url))
      case commandWithElement("navigateToDownload", url, uName, pass) =>
        val credentials = for {
          u <- Option(uName)
          p <- Option(pass)
        } yield Credentials(u, p)
        Some(NavigateToDownload(url, credentials))
      case "onCurrentPage" => Some(OnCurrentPage)
      case "up" => Some(Up)
      case commandWithParam("inAll", HtmlElementParser(elem)) => Some(InAll(elem))
      case commandWithParam("findContainingInLastResult", text) =>
        Some(FindContainingInLastResult(text))
      case commandWithNumber("waitSeconds", seconds) => Some(WaitSeconds(seconds.toInt))
      case commandWithParam("findLatestWithPrefix", prefix) => Some(FindLatestWithPrefix(prefix))
      case commandWithElementAndSubstring("findLatestByInnerText", HtmlElementParser(element), substring) =>
        Some(FindLatestByInnerText(element, substring))
      case _ => println("none?"); None
    }
  }
}
