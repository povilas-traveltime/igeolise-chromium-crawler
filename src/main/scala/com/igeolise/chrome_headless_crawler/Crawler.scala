package com.igeolise.chrome_headless_crawler

import java.io.File

import com.igeolise.chrome_headless_crawler.command_parser._
import org.slf4j.LoggerFactory

import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}

class Crawler(chromeSession: ChromeSession, timeout: FiniteDuration) {
  import CrawlerResult._

  private val log = LoggerFactory.getLogger(classOf[Crawler])

  @tailrec
  private def executeScripts(state: CrawlerState): CrawlerState = {
    state.unprocessedScripts match {
      case head :: tail =>
        val stateForExecution = state.copy(
          script = head,
          elementStack = Seq.empty,
          lazyLogger = LazyLog("Executing " + head.toString),
          unprocessedScripts = tail
        )
        val stateAfterExecution: CrawlerState = executeScript(stateForExecution)
        executeScripts(stateAfterExecution)
      case Nil => state
    }
  }

  def crawl(script: Script, downloadLocation: File): Either[CrawlerFailure, CrawlerSuccess] = {
    chromeSession.withSession { session =>
      val initialState = CrawlerState.createState(
        session,
        script,
        downloadLocation,
        timeout
      )
      val resultState = executeScripts(initialState)
      if (resultState.failures.nonEmpty) WithFailures(resultState.successes, resultState.failures)
      else Ok(resultState.successes)

    } match {
      case Success(result) => Right(result)
      case Failure(e) => Left(CrawlerFailure(e.getMessage))
    }
  }

  @tailrec
  private def executeScript(state: CrawlerState): CrawlerState = {
    state.stateWithNextAction match {
      case Some(s) =>
        val stateAfterAction = executeAction(s)
        executeScript(stateAfterAction)
      case None => state
    }
  }

  private def executeAction(state: CrawlerState): CrawlerState = {
    import StateActions.StateActionsExt

    state.script.getCurrentAction match {
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
    }
  }
}