package com.igeolise.chrome_headless_crawler

import java.io.File
import java.nio.file.Files
import java.util.Base64

import crawler.command_parser.Credentials
import io.webfolder.cdp.`type`.constant.DownloadBehavior
import io.webfolder.cdp.command.DOM
import io.webfolder.cdp.event.Events
import io.webfolder.cdp.event.network.LoadingFinished
import io.webfolder.cdp.event.page.LifecycleEvent
import io.webfolder.cdp.listener.EventListener
import io.webfolder.cdp.session.Session

import scala.collection.JavaConversions._
import scala.concurrent.{Await, Promise}
import scala.util.Try

object SessionHelpers {

  implicit class SessionHelpersExt(val session: Session) extends AnyVal {
    /**
      * This method is needed to make sure that sessions get closed on failures, otherwise chromium threads are left hanging.
      * @param failureMessage A message to add to the message of the exception thrown
      * @param command action to execute that might fail
      */
    def trySessionWithFailureMessage[A](failureMessage: String)(command: (Session) => A): Try[A] = {
      Try { command(session) }.recover { case e: Throwable =>
        session.close()
        throw CrawlerCaughtException(s"$failureMessage:\n${e.getMessage}", e)
      }
    }

    def setupDownloadBehaviour(target: File): Try[Unit] =
      trySessionWithFailureMessage("Setting download location") { ses: Session =>
        val page = ses.getCommand.getPage
        page.setDownloadBehavior(DownloadBehavior.Allow, target.getAbsolutePath)
      }

    def setCredentials(credentials: Credentials): Unit = {
      val network = session.getCommand.getNetwork
      network.enable()
      val headers: java.util.Map[String, Object] = Map[String, Object]("Authorization" -> ("Basic " + new String(Base64.getEncoder.encode(s"${credentials.user}:${credentials.password}".getBytes()))).asInstanceOf[Object])
      network.setExtraHTTPHeaders(headers)
    }

    def getDom(): DOM = {
      val dom = session.getCommand.getDOM
      dom.enable()
      session.waitDocumentReady()
      dom
    }
    def getNodeId(selector: String): Int = {
      session.getNodeId(selector)
    }

    def getDocumentNodeId(): Int = session.getDom().getDocument.getNodeId

    def waitForPage(action: (Session) => Unit, timeout: Int): Unit = {
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
      session.addEventListener(listener)
      action(session)
      Await.ready(promise.future, atMost = timeout.seconds)
      session.waitDocumentReady()
      session.removeEventEventListener(listener)
    }

    def getNodeAttributes(nodeId: Int): Map[String, String] = {
      val attributeList = session.getDom().getAttributes(nodeId).toList
      attributeList.grouped(2).map { case key::value::_ => key -> value }.toMap
    }

    def download(action: (Session) => Unit, target: File, timeout: Int): File = {
      val tempDir = Files.createTempDirectory("chr_crawler").toFile
      setupDownloadBehaviour(tempDir)
      session.waitForPage(action, timeout)
      val downloadedFile = tempDir.listFiles().headOption.getOrElse(throw CrawlerException("No file downloaded"))
      val resultFile = new File(target, downloadedFile.getName)
      Files.move(downloadedFile.toPath, resultFile.toPath)
      resultFile
    }
  }

}
