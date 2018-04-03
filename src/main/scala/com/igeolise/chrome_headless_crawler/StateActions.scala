package com.igeolise.chrome_headless_crawler

import com.igeolise.chrome_headless_crawler.command_parser.{Credentials, HtmlElement, In}

import scalaz.{-\/, \/-}

object StateActions {
  import com.igeolise.chrome_headless_crawler.CrawlerResult.FileWithLog
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
      state.copy(successes = FileWithLog(download, state.lazyLogger) +: state.successes)
    }

    def navigateTo(url: String): CrawlerState = {
      state.session.navigate(url)
      val documentId = state.session.getDocumentNodeId()
      state.copy(elementStack = Seq(documentId))
    }

    def navigateToDownload(url: String, credentials: Option[Credentials]): CrawlerState = {
        credentials.foreach(c=> state.session.setCredentials(c))
        val downloaded = state.session.download(_.navigate(url), state.target, state.timeout)
        state.copy(successes = FileWithLog(downloaded, state.lazyLogger) +: state.successes)
    }

    def forAllElems(element: HtmlElement): CrawlerState = {
      val newElements = for {
        nodeIds     <- state.session.getNodeIds(element.toSelectorString)
        attributes  <- nodeIds.map(n => state.session.getNodeAttributes(n))
        elements    <- attributes.map(a => Selectors.attributesToElement(a))
      } yield elements
      newElements match {
        case -\/(logEntry) => ???
        case \/-(elements) => state.expandScriptWithElements(elements)
      }
    }

    def expandScriptWithElements(elements: Seq[HtmlElement]): CrawlerState = {
      val newScripts = elements.map(e => state.script.replaceCurrentActionAndReset(In(e)))
      state.copy(unprocessedScripts = state.unprocessedScripts ++ newScripts)
    }

    def up: CrawlerState = state.stackTail
  }

}
