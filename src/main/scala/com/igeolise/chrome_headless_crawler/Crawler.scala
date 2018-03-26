package com.igeolise.chrome_headless_crawler

import java.io.File

import com.igeolise.chrome_headless_crawler.command_parser._
import org.slf4j.LoggerFactory

import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration

case class CrawlerResult(successes: Seq[File], failures: Seq[Seq[Action]])
case class CrawlerException(message: String) extends Exception(message)

class Crawler(chromeSession: ChromeSession, timeout: FiniteDuration) {

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
      case Nil => state
    }
  }

  def crawl(commands: Seq[Action], downloadLocation: File): CrawlerResult = {
    chromeSession.withSession { session =>
      val initialState = CrawlerState.createState(
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
    state.pendingActions.headOption match {
      case Some(a) =>
        val stateAfterAction = executeAction(a, state)
        executeScript(stateAfterAction)
      case None => state
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