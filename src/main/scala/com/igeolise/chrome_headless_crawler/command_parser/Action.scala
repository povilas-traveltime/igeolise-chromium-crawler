package com.igeolise.chrome_headless_crawler.command_parser

sealed abstract class Action(val actionName: String) {
  override def toString: String = actionName
}

sealed trait withElement extends Action {
  val element: HtmlElement

  override def toString: String = s"$actionName $element"
}

case class In(element: HtmlElement) extends Action("in") with withElement
case class TypeIn(text: String) extends Action("typeIn") { override def toString: String = s"$actionName $text" }
case object Click extends Action("click")
case object ClickDownload extends Action("clickDownload")
case class NavigateTo(url: String) extends Action("navigateTo") { override def toString: String = s"$actionName $url" }
case class NavigateToDownload(url: String, credentials: Option[Credentials]) extends Action("navigateToDownload") {
  override def toString: String = s"$actionName $url"
}
case object OnCurrentPage extends Action("onCurrentPage")
case object Up extends Action("up")
case class ForAllElems(element: HtmlElement) extends Action("forAllElems") with withElement
case class FindContainingInLastResult(text: String) extends Action("findContainingInLastResult") {
  override def toString: String = s"$actionName $text"
}

case class Credentials(user: String, password: String)
