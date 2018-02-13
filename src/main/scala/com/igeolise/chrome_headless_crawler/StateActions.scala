package com.igeolise.chrome_headless_crawler

import com.igeolise.chrome_headless_crawler.Selectors.toSelectorString
import crawler.command_parser.{HtmlElement, In}

import scala.util.Try

object StateActions {
  import SessionHelpers.SessionHelpersExt
  import io.webfolder.cdp.session.Extensions._
  implicit class StateActionsExt(val state: CrawlerState) extends AnyVal {

    def in(element: HtmlElement): CrawlerState = {
      for {
        id      <- state.session.getNodeId(toSelectorString(element))
        result  <- Try { state.appendStack(id) }
      } yield result
    }
    def click(): CrawlerState = {
      for {
        id      <- state.getStackTop
        _       <- state.session.waitForPage(_.clickDom(id), pageWaitTimeout)
        result  <- Try { state }
      } yield result
    }

    def onCurrentPage(): CrawlerState = {
      for {
        pageId  <- state.session.getDocumentNodeId()
        result  <- Try { state.copy(elementStack = Seq(pageId)) }
      } yield result
    }
    def typeIn(text: String): CrawlerState = {
      // refactor rest?
      val rest = (nodeId: Int) => state.session.trySessionWithFailureMessage("While typing") { ses =>
        ses.focus(nodeId)
        ses.sendKeys(text)
        state
      }
      for {
        nodeId <- state.getStackTop
        result <- rest(nodeId)
      } yield result
    }

    def findContainingInLastResult(text: String): CrawlerState = {
      for {
        nodes   <- state.session.trySessionWithFailureMessage("While getting node Ids") { _.getNodeIds(s"a:contains('$text')") }
        _       <- Try { nodes.map(n => state.session.trySessionWithFailureMessage("While clicking link") { _.clickDom(n) } ) }
        result  <- Try { state }
      } yield result
    }
    def clickDownload(): CrawlerState = {
      for {
        nodeId    <- state.getStackTop
        download  <- state.session.download(_.clickDom(nodeId), state.target, pageWaitTimeout)
        result    <- ??? //Try { state.copy(results = Some(download)) }
      } yield result
    }
    def navigateTo(url: String): CrawlerState = {
      for {
        _           <- state.session.trySessionWithFailureMessage(s"Navigating to $url") { _.navigate(url) }
        documentId  <- state.session.getDocumentNodeId()
        result      <- Try { state.copy(elementStack = Seq(documentId)) }
      } yield result
    }

    /// Option[(String, String)]
    def navigateToDownload(url: String, credentials: Option[(String, String)]): CrawlerState = {
      for {
        _ <- state.session.setCredentials(state.session, credentials)
        downloaded <- state.session.download(_.navigate(url), state.target, pageWaitTimeout)
        result <- Try { state.copy(successes = downloaded +: state.successes)}
      } yield result
    }
    def forAllElems(element: HtmlElement): CrawlerState = {
      val getNodeIds = (parrentId: Int) => state.session.trySessionWithFailureMessage("While getting node Ids") { ses =>
        ses.getNodeIds(toSelectorString(element))
      }
      for {
        parentNodeId  <- state.getStackTop
        nodes         <- getNodeIds(parentNodeId)
        attributes    <- Try { nodes.map(n => getNodeAttributes(state.session, n)).filter(_.isSuccess).map(_.get) }
        elements      <- Try { attributes.map(a => mapAttributesToElement(a)) }
        newStates     <- Try { elements.map(e => remapScriptWithoutForAll(state, sessionFactory, In(e)).get) }
      } yield newStates
    }
    def up: CrawlerState = {
      Try { state.stackTail }
    }


  }




}
