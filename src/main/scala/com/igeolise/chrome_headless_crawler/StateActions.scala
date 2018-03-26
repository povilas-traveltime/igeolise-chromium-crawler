package com.igeolise.chrome_headless_crawler

import java.io.File

import com.igeolise.chrome_headless_crawler.command_parser.{Action, Credentials, HtmlElement, In}
import io.webfolder.cdp.session.Session

import scala.concurrent.duration.FiniteDuration

object StateActions {
  import SessionHelpers.SessionHelpersExt
  import SessionExtensions._
  import Selectors._
  implicit class StateActionsExt(val state: CrawlerState) extends AnyVal {

    def in(element: HtmlElement): CrawlerState = {
      val id =  state.session.getNodeId(element.toSelectorString)
      state.appendStack(id)
    }

    def click(): CrawlerState = {
      val id =  state.getStackTop
      state.session.waitForPage(_.clickDom(id), state.timeout)
      state
    }

    def onCurrentPage(): CrawlerState = {
      val pageId = state.session.getDocumentNodeId()
      state.copy(elementStack = Seq(pageId))
    }

    def typeIn(text: String): CrawlerState = {
      val nodeId = state.getStackTop
      state.session.focus(nodeId)
      state.session.sendKeys(text)
      state
    }

    def findContainingInLastResult(text: String): CrawlerState = {
      val nodeIds = state.session.getNodeIds(s"a:contains('$text')")
      nodeIds.map(n => state.session.clickDom(n))
      state
    }

    def clickDownload(): CrawlerState = {
      val nodeId = state.getStackTop
      val download = state.session.download(_.clickDom(nodeId), state.target, state.timeout)
      state.copy(successes = download +: state.successes)
    }

    def navigateTo(url: String): CrawlerState = {
      state.session.navigate(url)
      val documentId = state.session.getDocumentNodeId()
      state.copy(elementStack = Seq(documentId))
    }

    def navigateToDownload(url: String, credentials: Option[Credentials]): CrawlerState = {
        credentials.foreach(c=> state.session.setCredentials(c))
        val downloaded = state.session.download(_.navigate(url), state.target, state.timeout)
        state.copy(successes = downloaded +: state.successes)
    }

    def forAllElems(element: HtmlElement): CrawlerState = {
      val nodeIds = state.session.getNodeIds(element.toSelectorString)
      val attributes = nodeIds.map(n => state.session.getNodeAttributes(n))
      val elements = attributes.map(a => Selectors.attributesToElement(a))
      state.expandScriptWithElements(elements)
    }

    def expandScriptWithElements(elements: Seq[HtmlElement]): CrawlerState = {
      val newScripts = elements.map(e => state.doneActions ++ (In(e) +: state.pendingActions))
      state.copy(unprocessedScripts = state.unprocessedScripts ++ newScripts, pendingActions = Seq.empty)
    }

    def up: CrawlerState = state.stackTail
  }

}
