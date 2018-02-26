package com.igeolise.chrome_headless_crawler

import java.io.File

import crawler.command_parser._
import org.slf4j.LoggerFactory

import scala.annotation.tailrec

case class CrawlerResult(successes: Seq[File], failures: Seq[Seq[Action]])
case class CrawlerException(message: String) extends Exception(message)
case class CrawlerCaughtException(message: String, exception: Throwable) extends Exception(s"$message:\n${exception.getMessage}", exception)

class Crawler(chromeSession: ChromeSession, timeout: Int) {

  private val log = LoggerFactory.getLogger(classOf[Crawler])

  @tailrec
  private def executeScripts(state: CrawlerState): CrawlerState = {
    state.unprocessedScripts match {
      case head :: tail =>
        val stateForExecution = state.copy(
          doneActions = Seq.empty,
          pendingActions = head,
          elementStack = Seq.empty,
          lazyLogger = LazyLog("Executing " + head.mkString("|")),
          unprocessedScripts = tail
        )
        val stateAfterExecution: CrawlerState = executeScript(stateForExecution)
        executeScripts(stateAfterExecution)
      case _ => state
    }
  }

  def crawl(commands: Seq[Action], downloadLocation: File): CrawlerResult = {
    chromeSession.withSession { session =>
      val initialState = StateActions.createState(
        session,
        commands,
        downloadLocation,
        timeout
      )
      val resultState = executeScripts(initialState)
      CrawlerResult(resultState.successes, resultState.failures)
    }
  }

  @tailrec
  private def executeScript(state: CrawlerState): CrawlerState = {
    state.pendingActions match {
      case a :: _ =>
        val stateAfterAction = executeAction(a, state)
        executeScript(stateAfterAction)
      case _ => state
    }
  }

  private def executeAction(action: Action, state: CrawlerState): CrawlerState = {
    import StateActions.StateActionsExt

    (action match {
      case In(element) => state.in(element)
      case Click => state.click()
      case ClickDownload => state.clickDownload()
      case OnCurrentPage => state.onCurrentPage()
      case Up => state.up
      case TypeIn(text) => state.typeIn(text)
      case FindContainingInLastResult(text) => state.findContainingInLastResult(text)
      case NavigateTo(url) => state.navigateTo(url)
      case NavigateToDownload(url, credentials) => state.navigateToDownload(url, credentials)
      case ForAllElems(element) => state.forAllElems(element)
    }).moveActionToDone
  }
}