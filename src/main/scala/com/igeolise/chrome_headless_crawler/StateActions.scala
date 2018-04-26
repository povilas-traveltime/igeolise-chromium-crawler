package com.igeolise.chrome_headless_crawler

import java.io.File
import com.igeolise.chrome_headless_crawler.CrawlerResult.ScriptWithLog
import com.igeolise.chrome_headless_crawler.command_parser.{Credentials, HtmlElement, In}
import com.igeolise.chrome_headless_crawler.model.{CrawlerState, ElementStack, LogEntry, ScriptState}
import WebElementExtensions._
import scalaz.{-\/, \/, \/-}
import scalaz.syntax.id._
import com.igeolise.chrome_headless_crawler.model.ComposedLenses._
import org.apache.http.client.utils.URIBuilder
import org.openqa.selenium.WebElement
import scalaz.syntax.traverse._
import scalaz.\/._
import scalaz.std.list._

object StateActions {
  import com.igeolise.chrome_headless_crawler.CrawlerResult.FileWithLog
  import Selectors._
  implicit class StateActionsExt(val state: CrawlerState) extends AnyVal {

    implicit def timeout = state.timeout

    def in(element: HtmlElement): CrawlerState = {
      state.driver.getElement(element.toSelectorString).leftMap(LogEntry) |>
      handleEither(e => elementStackL.modify(_.push(e)))
    }

    def click(): CrawlerState = {
      (for {
        elemAndStack  <- state.scriptState.elementStack.pop
        _             <- state.driver.waitForPage(elemAndStack._1.clickDisjunction).leftMap(LogEntry)
      } yield elemAndStack._2) |>
      handleEither(elementStackL.set)
    }

    def onCurrentPage(): CrawlerState = {
      state.driver.getDocumentRootNode.leftMap(s => LogEntry(s)) |>
      handleEither(id => elementStackL.set(ElementStack(List(id))))
    }

    def typeIn(text: String): CrawlerState = {
      (for {
        elementAndStack <- state.scriptState.elementStack.pop
        _               <- \/.fromTryCatchNonFatal(elementAndStack._1.sendKeys(text)).leftMap(e => LogEntry(s"Could not enter text in to ${elementAndStack._1.getTagName}, cause: ${e.getMessage}"))
      } yield elementAndStack._2) |> handleEither(elementStackL.set) _ // explicit conversion to function
    }

    def findContainingInLastResult(text: String): CrawlerState = {
      state.driver.getElements(s"a:contains('$text')").leftMap(LogEntry)
        .map(_.foreach(e => state.driver.download(e.clickDisjunction, state.target)
      )) |> handleEither(_ => identity)
    }

    def clickDownload(): CrawlerState = {
      (for {
        elementAndStack <- state.scriptState.elementStack.pop
        download        <- state.driver.download(elementAndStack._1.clickDisjunction, state.target).leftMap(LogEntry)
      } yield download) |> handleEither(addDownloadToState) _ // explicit conversion to a function
    }

    private def addDownloadToState(files: List[File]): CrawlerState => CrawlerState = { state: CrawlerState =>
      CrawlerState.successes.modify(_ ++ files.map(f => FileWithLog(
        f,
        state.scriptState.log.append(LogEntry(s"Succsesfully downloaded ${f.getAbsolutePath}"))
      ))) (state)
    }

    private def buildAuthUrl(url: String, credentials: Credentials): String = {
      val uri =  new URIBuilder(url)
      uri.setUserInfo(credentials.user, credentials.password)
      uri.build().toURL.toString
    }

    def navigateTo(url: String, credentials: Option[Credentials]): CrawlerState = {
      (for {
        _         <- credentials.map(c => state.driver.navigateTo(buildAuthUrl(url, c)).leftMap(LogEntry)).getOrElse(\/-(()))
        _         <- state.driver.navigateTo(url).leftMap(LogEntry)
        rootNode  <- state.driver.getDocumentRootNode.leftMap(LogEntry)
      } yield rootNode) |> handleEither(element => elementStackL.set(ElementStack(List(element))))
    }

    def navigateToDownload(url: String, credentials: Option[Credentials]): CrawlerState = {
      (for {
        _           <- credentials.map(c => state.driver.navigateTo(buildAuthUrl(url, c)).leftMap(LogEntry)).getOrElse(\/-(()))
        downloaded  <- state.driver.download(() => state.driver.navigateTo(url), state.target).leftMap(LogEntry)
      } yield downloaded) |> handleEither(addDownloadToState) _ // explicit conversion to function
    }

    private def getElementAttributes(element: WebElement): String \/ Map[String, String] = \/.fromTryCatchNonFatal {
      scala.xml.XML.loadString(element.getAttribute("outerHTML")).attributes.toList.map(a => a.key -> a.value.head.toString()).toMap
    }.leftMap(e => s"Could not get attributes from element ${element.getTagName}, cause: ${e.getMessage}")

    /**
      * Creates new scripts where each of the matching <element> is selected by an in action and ends execution of the current script
      * Emulates the behaviour of map or forEach
      * @param element element template for selection
      */
    def inAll(element: HtmlElement): CrawlerState = {
      (for {
        elements    <- state.driver.getElements(element.toSelectorString)
        attributes  <- elements.map(getElementAttributes).sequenceU
        newElements <- \/-(attributes.map(a => Selectors.attributesToElement(a)))
      } yield newElements).leftMap(LogEntry) |> handleEither {e: List[HtmlElement] => { s: CrawlerState =>
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
