package com.igeolise.chrome_headless_crawler.command_parser

sealed abstract class Action(val actionName: String) {
  override def toString: String = actionName
}

sealed trait withElement extends Action {
  val element: HtmlElement

  override def toString: String = s"$actionName $element"
}

import Action._

case class In(element: HtmlElement) extends Action(inN) with withElement
case class TypeIn(text: String) extends Action(typeInN) { override def toString: String = s"$actionName $text" }
case object Click extends Action(clickN)
case object ClickDownload extends Action(clickDownloadN)
case class NavigateTo(url: String) extends Action(navitateToN) { override def toString: String = s"$actionName $url" }
case class NavigateToDownload(url: String, credentials: Option[Credentials]) extends Action(navigateToDownloadN) {
  override def toString: String = s"$actionName $url"
}
case object OnCurrentPage extends Action(onCurrentPageN)
case object Up extends Action(upN)
case class InAll(element: HtmlElement) extends Action(inAllN) with withElement
case class FindContainingInLastResult(text: String) extends Action(findContainintInLastResultN) {
  override def toString: String = s"$actionName $text"
}

case class Credentials(user: String, password: String)

object Action {
//  Action name strings
  val inN = "in"
  val typeInN = "typeIn"
  val clickN = "click"
  val clickDownloadN = "clickDownload"
  val navitateToN = "navigateTo"
  val navigateToDownloadN = "navigateToDownload"
  val onCurrentPageN = "onCurrentPage"
  val upN = "up"
  val inAllN = "inAll"
  val findContainintInLastResultN = "findContainingInLastResult"
}