package com.igeolise.chrome_headless_crawler

import java.io.File

import com.igeolise.chrome_headless_crawler.command_parser._
import com.igeolise.chrome_headless_crawler.model._
import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}
import com.igeolise.chrome_headless_crawler.model.ComposedLenses._
import scalaz.{-\/, \/, \/-}
import scalaz.syntax.id._

class Crawler(chromeDriverFile: File, downloadLocation: File, timeout: FiniteDuration) {
  import CrawlerResult._

  @tailrec
  private def executeScripts(state: CrawlerState): CrawlerState = {
    val firstState = executeScript(state)
    firstState.unprocessedScripts match {
      case head :: tail =>
        val stateAfterExecution = state |>
          CrawlerState.scriptState.set(ScriptState(head, ElementStack(List.empty), LazyLog(s"Executing ${head.toString}"))) |>
          CrawlerState.unprocessedScripts.set(tail) |>
          executeScript
        executeScripts(stateAfterExecution)
      case Nil => state
    }
  }

  def crawl(script: Script): \/[CrawlerFailure, CrawlerResults[LazyLog, Script]] = {
    ChromiumDriver.withDriver(chromeDriverFile) { driver =>
      val initialState = CrawlerStateFactory.createState(
        driver,
        script,
        downloadLocation,
        timeout
      )
      val resultState = executeScripts(initialState)
      CrawlerResults(resultState.successes, resultState.failures)

    } match {
      case Success(result) => \/-(result)
      case Failure(e) => -\/(CrawlerFailure(e.getMessage))
    }
  }

  @tailrec
  private def executeScript(state: CrawlerState): CrawlerState = {
    state.scriptState.script.getAction match {
      case None => state
      case Some(action) =>
        val newState = executeAction(state, action)
        executeScript(newState)
    }
  }

  private def executeAction(state: CrawlerState, action: Action): CrawlerState = {
    import StateActions.StateActionsExt
    val stateWithLog = scriptLogL.modify(_.append(LogEntry(s"Executing action '${action.toString}'"))) (state)

    (action match {
      case In(element) => stateWithLog.in(element)
      case Click => stateWithLog.click()
      case ClickNoWait => stateWithLog.clickNoWait()
      case ClickDownload => stateWithLog.clickDownload()
      case OnCurrentPage => stateWithLog.onCurrentPage()
      case Up => stateWithLog.up
      case TypeIn(text) => stateWithLog.typeIn(text)
      case FindContainingInLastResult(text) => stateWithLog.findContainingInLastResult(text)
      case NavigateTo(url) => stateWithLog.navigateTo(url, None)
      case NavigateToDownload(url, credentials) => stateWithLog.navigateToDownload(url, credentials)
      case InAll(element) => stateWithLog.inAll(element)
      case WaitSeconds(secs) => stateWithLog.waitSeconds(secs)
      case FindLatestWithPrefix(prefix) => stateWithLog.findLatestWithPrefix(prefix)
      case FindLatestByInnerText(element, prefix) => stateWithLog.findLatestByInnerText(element, prefix)
    }) |> currentScriptL.modify(_.withNextAction)
  }
}