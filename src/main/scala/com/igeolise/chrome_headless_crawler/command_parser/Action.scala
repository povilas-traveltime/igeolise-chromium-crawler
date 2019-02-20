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
case object ClickNoWait extends Action(clickNoWaitN)
case object ClickDownload extends Action(clickDownloadN)
case class NavigateTo(url: String) extends Action(navigateToN) { override def toString: String = s"$actionName $url" }
case class NavigateToDownload(url: String, credentials: Option[Credentials]) extends Action(navigateToDownloadN) {
  override def toString: String = s"$actionName $url"
}
case object OnCurrentPage extends Action(onCurrentPageN)
case object Up extends Action(upN)
case class InAll(element: HtmlElement) extends Action(inAllN) with withElement
case class FindContainingInLastResult(text: String) extends Action(findContainingInLastResultN) {
  override def toString: String = s"$actionName $text"
}
case class WaitSeconds(seconds: Int) extends Action(waitSecondsN)
case class FindLatestWithPrefix(prefix: String) extends Action(findLatestWithPrefixN)
case class FindLatestByInnerText(element: HtmlElement, substring: String) extends Action(findLatestByInnerTextN)

case class Credentials(user: String, password: String)

object Action {
//  Action name strings
  val inN = "in"
  val typeInN = "typeIn"
  val clickN = "click"
  val clickNoWaitN = "clickNoWait"
  val clickDownloadN = "clickDownload"
  val navigateToN = "navigateTo"
  val navigateToDownloadN = "navigateToDownload"
  val onCurrentPageN = "onCurrentPage"
  val upN = "up"
  val inAllN = "inAll"
  val findContainingInLastResultN = "findContainingInLastResult"
  val waitSecondsN = "waitSeconds"
  val findLatestWithPrefixN = "findLatestWithPrefix"
  val findLatestByInnerTextN = "findLatestByInnerText"
}