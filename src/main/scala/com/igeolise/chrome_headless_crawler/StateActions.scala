package com.igeolise.chrome_headless_crawler

import java.io.File

import com.igeolise.chrome_headless_crawler.CrawlerResult.ScriptWithLog
import com.igeolise.chrome_headless_crawler.command_parser.{Credentials, Discriminator, HtmlElement, In}
import com.igeolise.chrome_headless_crawler.model.{CrawlerState, ElementStack, LogEntry, ScriptState}
import WebElementExtensions._
import cats.implicits.toBifunctorOps
import com.igeolise.Helpers._
import com.igeolise.chrome_headless_crawler.model.ComposedLenses._
import com.softwaremill.quicklens._
import org.apache.http.client.utils.URIBuilder
import org.openqa.selenium.WebElement
import cats.syntax.traverse._
import cats.instances
import instances.{either, list, vector}
import either._
import list._

import scala.util.Try

object StateActions {
  import com.igeolise.chrome_headless_crawler.CrawlerResult.FileWithLog
  import Selectors._
  implicit class StateActionsExt(val state: CrawlerState) extends AnyVal {

    implicit def timeout = state.timeout

    def in(element: HtmlElement): CrawlerState = {
      state.scriptState.elementStack.pop.flatMap { case (element_, _) =>
        element_.findElementByXpath(element.toSelectorString).leftMap(LogEntry)
      } |>
        handleEither(e => elementStackL.using(_.push(e)))
    }

    def click(): CrawlerState = {
      (for {
        elemAndStack  <- state.scriptState.elementStack.pop
        _             <- state.driver.waitForPage(() => elemAndStack._1.clickEither(state.driver.driver)).leftMap(LogEntry)
      } yield elemAndStack._2) |>
      handleEither(elementStackL.setTo) _ // explicit conversion to function
    }

    def clickNoWait(): CrawlerState = {
      (for {
        elemAndStack  <- state.scriptState.elementStack.pop
        _             <- elemAndStack._1.clickEither(state.driver.driver).leftMap(LogEntry)
      } yield elemAndStack._2) |>
        handleEither(elementStackL.setTo) _ // explicit conversion to function
    }

    def onCurrentPage(): CrawlerState = {
      state.driver.getDocumentRootNode.leftMap(s => LogEntry(s)) |>
      handleEither(id => elementStackL.setTo(ElementStack(List(id))))
    }

    def typeIn(text: String): CrawlerState = {
      (for {
        elementAndStack <- state.scriptState.elementStack.pop
        _               <- Try (elementAndStack._1.sendKeys(text)).leftMap(e => LogEntry(s"Could not enter text in to ${elementAndStack._1.getTagName}, cause: ${e.getMessage}"))
      } yield elementAndStack._2) |> handleEither(elementStackL.setTo) _ // explicit conversion to function
    }

    def findContainingInLastResult(text: String): CrawlerState = {
      (for {
        elements    <- state.scriptState.elementStack.pop.flatMap{ case (element, stack) => element.findElementsByXpath(s"""//a[contains(@*, '$text') or contains(text(), '$text')]""").leftMap(LogEntry)}
        downloaded  <- elements.map(e => state.driver.download(() => e.clickEither(state.driver.driver), state.target)).sequence.leftMap(LogEntry)
      } yield downloaded.map(f =>
          FileWithLog(f, state.scriptState.log)
      )) |>
        handleEither(results => modify(_: CrawlerState)(_.successes).using(_ ++ results)
      )
    }

    def clickDownload(): CrawlerState = {
      (for {
        elementAndStack <- state.scriptState.elementStack.pop
        download        <- state.driver.download(() => elementAndStack._1.clickEither(state.driver.driver), state.target).leftMap(LogEntry)
      } yield download) |> handleEither(addDownloadToState) _ // explicit conversion to a function
    }

    private def addDownloadToState(file: File): CrawlerState => CrawlerState = { state: CrawlerState =>
      state.modify(_.successes).using(_ :+
        FileWithLog(
          file,
          state.scriptState.log.append(LogEntry(s"Succsesfully downloaded ${file.getAbsolutePath}")
          )
        )
      )
    }

    private def buildAuthUrl(url: String, credentials: Credentials): String = {
      val uri =  new URIBuilder(url)
      uri.setUserInfo(credentials.user, credentials.password)
      uri.build().toURL.toString
    }

    def navigateTo(url: String, credentials: Option[Credentials]): CrawlerState = {
      (for {
        _         <- credentials.map(c => state.driver.navigateTo(buildAuthUrl(url, c)).leftMap(LogEntry)).getOrElse(Right(()))
        _         <- state.driver.navigateTo(url).leftMap(LogEntry)
        rootNode  <- state.driver.getDocumentRootNode.leftMap(LogEntry)
      } yield rootNode) |> handleEither(element => elementStackL.setTo(ElementStack(List(element))))
    }

    def navigateToDownload(url: String, credentials: Option[Credentials]): CrawlerState = {
      (for {
        _           <- credentials.map(c => state.driver.navigateTo(buildAuthUrl(url, c)).leftMap(LogEntry)).getOrElse(Right(()))
        downloaded  <- state.driver.download(() => state.driver.navigateTo(url), state.target).leftMap(LogEntry)
      } yield downloaded) |> handleEither(addDownloadToState) _ // explicit conversion to function
    }

    private def getElementAttributes(element: WebElement): Either[String, Map[String, String]] = Try {
      scala.xml.XML.loadString(element.getAttribute("outerHTML")).attributes.toList.map(a => a.key -> a.value.head.toString()).toMap
    }.leftMap(e => s"Could not get attributes from element ${element.getTagName}, cause: ${e.getMessage}")

    /**
      * Creates new scripts where each of the matching <element> is selected by an in action and ends execution of the current script
      * Emulates the behaviour of map or forEach
      * @param element element template for selection
      */
    def inAll(element: HtmlElement): CrawlerState = {
      (for {
        stackAndElement <- state.scriptState.elementStack.pop
        elements        <- stackAndElement._1.findElementsByXpath(element.toSelectorString).leftMap(LogEntry)
        attributes      <- elements.map(e => getElementAttributes(e).map(a => a -> e)).sequence.leftMap(LogEntry)
        newElements     = attributes.map { case (attributes_, element_) => Selectors.attributesToElement(attributes_, element_.getTagName)}
      } yield newElements) |> handleEither {e: List[HtmlElement] => { s: CrawlerState =>
        s.expandScriptWithElements(e) |> currentScriptL.using(_.endScript)
      } }
    }

    /**
      * Injects new element select actions instead of the current action in the script forming new scripts.
      * @param elements elements to select
      */
    def expandScriptWithElements(elements: Seq[HtmlElement]): CrawlerState = {
      val newScripts = elements.map(e => state.scriptState.script.replaceCurrentActionAndReset(In(e)))
      state.modify(_.unprocessedScripts).using(_ ++ newScripts)
    }

    def up: CrawlerState = {
      state.scriptState.elementStack.pop.map(_._2) |>
      handleEither(s => elementStackL.setTo(s))
    }

    def waitSeconds(duration: Int): CrawlerState = {
      Thread.sleep(duration * 1000)
      state |> scriptLogL.using(_.append(LogEntry(s"Waiting $duration seconds.")))
    }

    def findLatestByInnerText(element: HtmlElement, substring: String): CrawlerState = {
      (for {
        elementAndStack   <- state.scriptState.elementStack.pop
        foundElements     <- elementAndStack._1.findElementsByXpath(element.toSelectorString).leftMap(LogEntry)
        filteredElements  <- filterElementsByInnerText(foundElements, _.contains(substring))
        maxElement        <- Try( filteredElements.maxBy(e => cutToTail(e.getText, substring)) )
                               .leftMap(_ => LogEntry("Specified element not found."))
      } yield maxElement) |> handleEither(element => elementStackL.setTo(ElementStack(List(element))))
    }

    def findLatestWithPrefix(prefix: String): CrawlerState = {
      (for {
        elementAndStack           <- state.scriptState.elementStack.pop
        foundElements             <- elementAndStack._1.findElementsByXpath("//a").leftMap(LogEntry)
        filteredElements          <- filterElements(foundElements, "href", _.contains(prefix))
        resultElement             <- Try (filteredElements.maxBy { e => cutToTail(e.getAttribute("href"), prefix) }).leftMap(_ => LogEntry("Specified element not found."))
        download                  <- state.driver.download(() => resultElement.clickEither(state.driver.driver), state.target).leftMap(LogEntry)
      } yield download) |> handleEither(addDownloadToState) _ // explicit conversion to function
    }

    /***
      * Finds the last occurrence of @slice and drops everything before its end (including the slice).
      */
    private def cutToTail(input: String, slice: String): String = {
      val startOfPrefix = input.lastIndexOf(slice)
      input.drop(startOfPrefix).stripPrefix(slice)
    }

    private def filterElementsByInnerText(elements: Traversable[WebElement], predicate: String => Boolean): Either[LogEntry, List[WebElement]] = {
      Try (elements.filter{ e =>
        val textValue = e.getText
        textValue != null && predicate(textValue)
      }.toList).leftMap(e => LogEntry(s"Error while filtering elements by inner text, message: ${e.getMessage}"))
    }

    private def filterElements(elements: Traversable[WebElement], attributeName: String, predicate: String => Boolean): Either[LogEntry, List[WebElement]] = {
      Try (elements.filter{ e =>
        val attribute = e.getAttribute(attributeName)
        attribute != null && predicate(attribute)
      }.toList).leftMap(e => LogEntry(s"Error while filtering elements, message: ${e.getMessage}"))
    }

    private def handleEither[A](modification: A => (CrawlerState => CrawlerState))
      (result: Either[LogEntry,A]): CrawlerState = {
      result match {
        case Left(logEntry) =>
          val failureLog = state.scriptState.log.append(logEntry)
          state
            .modify(_.failures).using(_ :+ ScriptWithLog(state.scriptState.script, failureLog))
            .modify(_.scriptState.script).using(_.endScript)
        case Right(r) => state |> modification(r)
      }
    }
  }
}
