package com.igeolise.chrome_headless_crawler

import java.io.File
import java.nio.file.Files
import java.util.Base64
import java.util.function.Predicate

import crawler.command_parser.{Action, CustomSelector, HtmlElement}
import io.webfolder.cdp.`type`.constant.DownloadBehavior
import io.webfolder.cdp.command.DOM
import io.webfolder.cdp.event.Events
import io.webfolder.cdp.event.network.LoadingFinished
import io.webfolder.cdp.event.page.LifecycleEvent
import io.webfolder.cdp.listener.EventListener
import io.webfolder.cdp.session.{Session, SessionFactory}

import scala.collection.JavaConversions._
import scala.concurrent.{Await, Promise}
import scala.util.Try

trait CrawlerHelpers {

  /**
    * This method is needed to make sure that sessions get closed on failures, otherwise chromium threads are left hanging.
    * @param failureMessage A message to add to the message of the exception thrown
    * @param command action to execute that might fail
    */
  protected def trySessionWithFailureMessage[A](failureMessage: String, session: Session)(command: (Session) => A): Try[A] = {
    Try { command(session) }.recover { case e: Throwable =>
      session.close()
      throw CrawlerCaughtException(s"$failureMessage:\n${e.getMessage}", e)
    }
  }

  protected def setupDownloadBehaviour(session: Session, target: File): Try[Unit] =
    trySessionWithFailureMessage("Setting download location", session) { ses: Session =>
      val page = ses.getCommand.getPage
      page.setDownloadBehavior(DownloadBehavior.Allow, target.getAbsolutePath)
    }

  protected def setCredentials(session: Session, credentials: Option[(String, String)]): Try[Unit] =
    trySessionWithFailureMessage("Setting credentials", session) { ses: Session =>
      val network = ses.getCommand.getNetwork
      network.enable()
      credentials.foreach { case (username, password) =>
        val headers: java.util.Map[String, Object] = Map[String, Object]("Authorization" -> ("Basic " + new String(Base64.getEncoder.encode(s"$username:$password".getBytes()))).asInstanceOf[Object])
        network.setExtraHTTPHeaders(headers)
      }
    }

  protected def getDom(session: Session): Try[DOM] =
    trySessionWithFailureMessage("Getting DOM", session) { ses =>
      val dom = ses.getCommand.getDOM
      dom.enable()
      ses.waitDocumentReady()
      dom
    }
  protected def getNodeId(session: Session, selector: String): Try[Int] =
    trySessionWithFailureMessage(s"Getting node id for $selector", session) { ses =>
      ses.getNodeId(selector)
    }

  protected def getDocumentNodeId(session: Session): Try[Int] =
    for {
      dom <- getDom(session)
      id  <- trySessionWithFailureMessage("While getting Document node id", session) { _ => dom.getDocument.getNodeId }
    } yield id

  protected def waitForPage(action: (Session) => Unit, timeout: Int) (session: Session): Try[Unit] = {
    import  scala.concurrent.duration._

    val promise = Promise[Unit]()


    val listener = new EventListener[AnyRef] {
      override def onEvent(event: Events, value: scala.AnyRef): Unit = {
        value match {
          case e: LoadingFinished => promise.success(())
          case e: LifecycleEvent if e.getName == "networkIdle" => promise.success(())
          case _ =>
        }
      }
    }

    trySessionWithFailureMessage("Waiting for document to be ready", session) { ses =>
      ses.addEventListener(listener)
      action(ses)
      Await.ready(promise.future, atMost = timeout.seconds)
      ses.wait(100)
      ses.waitDocumentReady()
      ses.removeEventEventListener(listener)
    }
  }

  protected def getNodeAttributes(session: Session, nodeId: Int): Try[Map[String, String]] = {
    val transformListToMap = (list: Iterable[String]) =>
      Try { list.grouped(2).map { case key::value::_ => key -> value }.toMap }
        .recover { case e: Throwable => throw CrawlerCaughtException(s"While making attribute map:\n${e.getMessage}", e) }
    for {
      dom <- getDom(session)
      attributeList <- trySessionWithFailureMessage("While getting node attributes", session) { _ => dom.getAttributes(nodeId) }
      attributeMap  <- transformListToMap(attributeList)
    } yield attributeMap
  }

  protected def mapAttributesToElement(attributes: Map[String, String]): HtmlElement = {
    val textContentSelector = attributes.get("textContent").map(t => s"textContent='$t'").getOrElse("")
    val hrefSelector = attributes.get("href").map("href=" + _).getOrElse("")
    val idSelector = attributes.get("id").map("id" + _).getOrElse("")
    val combined = Seq(textContentSelector, hrefSelector, idSelector).mkString(",")
    CustomSelector(s"*[$combined]")

  }

  protected def download(session: Session, action: (Session) => Unit, target: File, timeout: Int): Try[File] = {
    val createTempDir = Try { Files.createTempDirectory("chr_crawler").toFile }
    val getDownloadedFile = (dempDir: File) => Try {
      dempDir.listFiles().headOption.getOrElse(throw CrawlerException("No file downloaded"))
    }
    val moveFile = (file: File, target: File) => Try {
      Files.move(file.toPath, new File(target, file.getName).toPath)
    }

    for {
      tempDir     <- createTempDir
      _           <- setupDownloadBehaviour(session, tempDir)
      _           <- waitForPage(action, timeout) (session)
      downloaded  <- getDownloadedFile(tempDir)
      _           <- moveFile(downloaded, target)
    } yield downloaded
  }

}
