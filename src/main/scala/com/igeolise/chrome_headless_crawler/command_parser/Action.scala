package crawler.command_parser

sealed trait Action

case class In(element: HtmlElement) extends Action
case class TypeIn(text: String) extends Action
case object Click extends Action
case object ClickDownload extends Action
case class NavigateTo(url: String) extends Action
case class NavigateToDownload(url: String, credentials: Option[(String, String)]) extends Action
case object OnCurrentPage extends Action
case object Up extends Action
case class ForAllElems(element: HtmlElement) extends Action
case class FindContainingInLastResult(text: String) extends Action
