package com.igeolise.chrome_headless_crawler

import java.io.File

import com.igeolise.chrome_headless_crawler.command_parser._
import com.igeolise.chrome_headless_crawler.model._
import org.slf4j.LoggerFactory

import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}
import com.igeolise.chrome_headless_crawler.model.ComposedLenses._
import scalaz.syntax.id._

class Crawler(chromeSession: ChromeSession, timeout: FiniteDuration) {
  import CrawlerResult._

  private val log = LoggerFactory.getLogger(classOf[Crawler])

  @tailrec
  private def executeScripts(state: CrawlerState): CrawlerState = {
    state.unprocessedScripts match {
      case head :: tail =>
        val stateAfterExecution = state |>
          CrawlerState.scriptState.set(ScriptState(head, ElementStack(List.empty), LazyLog(s"Executing ${head.toString}"))) |>
          CrawlerState.unprocessedScripts.set(tail) |>
          executeScript
        executeScripts(stateAfterExecution)
      case Nil => state
    }
  }

  def crawl(script: Script, downloadLocation: File): Either[CrawlerFailure, CrawlerResults[LazyLog, Script]] = {
    chromeSession.withSession { session =>
      val initialState = CrawlerStateFactory.createState(
        session,
        script,
        downloadLocation,
        timeout
      )
      val resultState = executeScripts(initialState)
      CrawlerResults(resultState.successes, resultState.failures)

    } match {
      case Success(result) => Right(result)
      case Failure(e) => Left(CrawlerFailure(e.getMessage))
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
      case ClickDownload => stateWithLog.clickDownload()
      case OnCurrentPage => stateWithLog.onCurrentPage()
      case Up => stateWithLog.up
      case TypeIn(text) => stateWithLog.typeIn(text)
      case FindContainingInLastResult(text) => stateWithLog.findContainingInLastResult(text)
      case NavigateTo(url) => stateWithLog.navigateTo(url)
      case NavigateToDownload(url, credentials) => stateWithLog.navigateToDownload(url, credentials)
      case InAll(element) => stateWithLog.forAllElems(element)
    }) |> currentScriptL.modify(_.withNextAction)
  }
}