package com.igeolise.chrome_headless_crawler

import java.io.File

import com.igeolise.Helpers.{IdOps, TryOps}
import com.igeolise.chrome_headless_crawler.command_parser._
import com.igeolise.chrome_headless_crawler.model._
import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
import com.igeolise.chrome_headless_crawler.model.ComposedLenses._
import com.softwaremill.quicklens._


class Crawler(chromeDriverFile: File, chromeFile: File, logFile: File, downloadLocation: File, timeout: FiniteDuration) {
  import CrawlerResult._

  @tailrec
  private def executeScripts(state: CrawlerState): CrawlerState = {
    val firstState = executeScript(state)
    firstState.unprocessedScripts match {
      case head :: tail =>
        val nextState = {
          firstState
            .modify(_.scriptState).setTo(ScriptState(head, ElementStack(List.empty), LazyLog(s"Executing ${head.toString}")))
            .modify(_.unprocessedScripts).setTo(tail) |> executeScript
        }
        executeScripts(nextState)
      case Nil => firstState
    }
  }

  def crawl(script: Script): Either[CrawlerFailure, CrawlerResults[LazyLog, Script]] = {
    ChromiumDriver.withDriver(chromeDriverFile, chromeFile, logFile, downloadLocation) { driver =>
      val initialState = CrawlerStateFactory.createState(
        driver,
        script,
        downloadLocation.getCanonicalFile,
        timeout
      )
      val resultState = executeScripts(initialState)
      CrawlerResults(resultState.successes, resultState.failures)

    }.leftMap(e => CrawlerFailure(e.getMessage))
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
    val stateWithLog = scriptLogL.using(_.append(LogEntry(s"Executing action '${action.toString}'"))) (state)

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
      case FindLatestByInnerText(element, substring) => stateWithLog.findLatestByInnerText(element, substring)
    }) |> currentScriptL.using(_.withNextAction)
  }
}