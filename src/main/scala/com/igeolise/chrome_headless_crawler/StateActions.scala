package com.igeolise.chrome_headless_crawler

import java.io.File

import com.igeolise.chrome_headless_crawler.CrawlerResult.ScriptWithLog
import com.igeolise.chrome_headless_crawler.command_parser.{Credentials, HtmlElement, In}
import com.igeolise.chrome_headless_crawler.model.{CrawlerState, ElementStack, LogEntry, ScriptState}

import scalaz.{-\/, \/, \/-}
import scalaz.syntax.id._
import com.igeolise.chrome_headless_crawler.model.ComposedLenses._

import scalaz.syntax.traverse._
import scalaz.\/._
import scalaz.std.list._

object StateActions {
  import com.igeolise.chrome_headless_crawler.CrawlerResult.FileWithLog
  import SessionHelpers.SessionHelpersExt
  import SessionExtensions._
  import Selectors._
  implicit class StateActionsExt(val state: CrawlerState) extends AnyVal {

    def in(element: HtmlElement): CrawlerState = {
      val id =  state.session.getNodeId(element.toSelectorString)
      state |> elementStackL.modify(_.push(id))
    }

    def click(): CrawlerState = {
      (for {
        idAndStack <- state.scriptState.elementStack.pop
        _          <- state.session.waitForPage(_.clickDom(idAndStack._1), state.timeout)
      } yield idAndStack._2) |>
      handleEither(elementStackL.set)
    }

    def onCurrentPage(): CrawlerState = {
      state.session.getDocumentNodeId() |>
      handleEither(id => elementStackL.set(ElementStack(List(id))))
    }

    def typeIn(text: String): CrawlerState = {
      (for {
        idAndStack <- state.scriptState.elementStack.pop
        _               <- state.session.focus(idAndStack._1)
        _               <- state.session.typeIn(text)
      } yield idAndStack._2) |> handleEither(elementStackL.set) _
    }

    def findContainingInLastResult(text: String): CrawlerState = {
      state.session.getNodeIds(s"a:contains('$text')").flatMap(_.map(
        n => state.session.clickDom(n)
      ).sequenceU) |> handleEither(_ => identity)
    }

    def clickDownload(): CrawlerState = {
      (for {
        stackAndId    <- state.scriptState.elementStack.pop
        download  <- state.session.download(_.clickDom(stackAndId._1), state.target, state.timeout)
      } yield download) |> handleEither(addDownloadToState) _ // explicit conversion to a function
    }

    private def addDownloadToState(f: File): CrawlerState => CrawlerState = { state: CrawlerState =>
      CrawlerState.successes.modify(_ :+ FileWithLog(
        f,
        state.scriptState.log.append(LogEntry(s"Succsesfully downloaded ${f.getAbsolutePath}"))
      )) (state)
    }

    def navigateTo(url: String): CrawlerState = {
      (for {
        _       <- \/.fromTryCatchNonFatal(state.session.navigate(url)).leftMap(_ => LogEntry(s"Failed to navigate to: $url"))
        nodeId  <- state.session.getDocumentNodeId()

      } yield nodeId) |>
      handleEither(id => elementStackL.set(ElementStack(List(id))))
    }

    private def setCredentials(credentials: Option[Credentials], state: CrawlerState): LogEntry \/ Unit = {
      credentials match {
        case None => \/-(())
        case Some(c) => state.session.setCredentials(c)
      }
    }

    def navigateToDownload(url: String, credentials: Option[Credentials]): CrawlerState = {
      (for {
        _           <- setCredentials(credentials, state)
        downloaded  <- state.session.download(_.navigate(url), state.target, state.timeout)
      } yield downloaded) |> handleEither(addDownloadToState)
    }

    /**
      * Creates new scripts where each of the matching <element> is selected by an in action and ends execution of the current script
      * Emulates the behaviour of map or forEach
      * @param element element template for selection
      */
    def forAllElems(element: HtmlElement): CrawlerState = {
      (for {
        nodeIds     <- state.session.getNodeIds(element.toSelectorString)
        attributes  <- nodeIds.map(n => state.session.getNodeAttributes(n)).sequenceU
        elements    <- \/-(attributes.map(a => Selectors.attributesToElement(a)))
      } yield elements) |> handleEither {e: List[HtmlElement] => { s: CrawlerState =>
        s.expandScriptWithElements(e) |> currentScriptL.modify(_.endScript)
      } }
    }

    /**
      * Injects new element select actions instead of the current action in the script forming new scripts.
      * @param elements elements to select
      */
    def expandScriptWithElements(elements: Seq[HtmlElement]): CrawlerState = {
      val newScripts = elements.map(e => state.scriptState.script.replaceCurrentActionAndReset(In(e)))
      state |> CrawlerState.unprocessedScripts.modify(_ ++ newScripts)
    }

    def up: CrawlerState = {
      state.scriptState.elementStack.pop.map(_._2) |>
      handleEither(s => elementStackL.set(s))
    }

    private def handleEither[A](modification: A => (CrawlerState => CrawlerState))
      (result: LogEntry \/ A): CrawlerState = {
      result match {
        case -\/(logEntry) =>
          val failureLog = state.scriptState.log.append(logEntry)
          state |>
            CrawlerState.failures.modify(_ :+ ScriptWithLog(state.scriptState.script, failureLog)) |>
            currentScriptL.modify(_.endScript)
        case \/-(r) => state |> modification(r)
      }
    }

  }

}
