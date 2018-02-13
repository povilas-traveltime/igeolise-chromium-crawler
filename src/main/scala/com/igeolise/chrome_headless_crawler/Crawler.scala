package com.igeolise.chrome_headless_crawler

import java.io.File

import com.igeolise.chrome_headless_crawler.Selectors.toSelectorString
import crawler.command_parser._
import io.webfolder.cdp.session.Session
import io.webfolder.cdp.session.Extensions._
import org.slf4j.LoggerFactory

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

case class CrawlerResult(successes: Seq[File], failures: Seq[Action])
case class CrawlerException(message: String) extends Exception(message)
case class CrawlerCaughtException(message: String, exception: Throwable) extends Exception(s"$message:\n${exception.getMessage}", exception)

class Crawler(chromeSession: ChromeSession, pageWaitTimeout: Int) extends SessionHelpers {

  private val log = LoggerFactory.getLogger(classOf[Crawler])

  private def createState(session: Session, actionSequence: Seq[Action], downloadTarget: File): Try[CrawlerState] = {
    val initState = (documentId: Int) => Try {
      CrawlerState(
        Seq(documentId), Seq.empty, actionSequence, LazyLog("Crawling " + actionSequence.mkString("|")),
        downloadTarget, session, Seq.empty, Seq.empty, Seq.empty
      )
    }
    for {
      _           <- waitForPage(_.navigate(url), pageWaitTimeout)(session)
      documentId  <- getDocumentNodeId(session)
      state       <- initState(documentId)
    } yield state
  }

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