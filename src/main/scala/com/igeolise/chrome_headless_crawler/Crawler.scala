package com.igeolise.chrome_headless_crawler

import java.io.File

import crawler.command_parser._
import org.slf4j.LoggerFactory

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

case class CrawlerResult(successes: Seq[File], failures: Seq[Action])
case class CrawlerException(message: String) extends Exception(message)
case class CrawlerCaughtException(message: String, exception: Throwable) extends Exception(s"$message:\n${exception.getMessage}", exception)

class Crawler(chromeSession: ChromeSession, timeout: Int) {

  private val log = LoggerFactory.getLogger(classOf[Crawler])

  @tailrec
  private def executeScript(state: CrawlerState): CrawlerState = {
    val newState: CrawlerState = state.pendingActions.foldLeft(state) { case (state_, action) =>
        executeAction(action, state_)
    }

    newState.unprocessedScripts match {
      case head :: tail =>
        val stateForExecution: CrawlerState = ???
        executeScript(stateForExecution)
      case _ => newState
    }
  }

  def crawl(commands: Seq[Action], downloadLocation: File): CrawlerResult = {
    chromeSession.withSession { session =>

      val initialState = StateActions.up
    }




    commands.headOption match {
      case Some(NavigateTo(url)) =>

        val result = for {
          state       <- createState(session, commands.tail, downloadLocation, url)
          result_     <- Try { executeActions(Seq(state)) }
        } yield result_

        result match {
          case Success(r) => manageResult(r)
          case Failure(e) => manageResult(Seq(Try(throw CrawlerCaughtException("Problem while initializing",e))))
        }

      case Some(NavigateToDownload(url, credentials)) =>
        val session = sessionFactory.create()
        val result =

        result match {
          case Success(r) => CrawlerResult(Seq(r), Seq.empty)
          case Failure(e) => CrawlerResult(Seq.empty, Seq(e))
        }

      case Some(_) => CrawlerResult(Seq.empty, Seq(CrawlerException(s"First command does not navigate to a page:\n" +
        commands.map(_.toString).mkString("\\"))))
      case None => CrawlerResult(Seq.empty, Seq(CrawlerException("Action list empty")))
    }
  }

  private def executeAction(action: Action, state: CrawlerState): CrawlerState = {
    import StateActions.StateActionsExt

    action match {
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