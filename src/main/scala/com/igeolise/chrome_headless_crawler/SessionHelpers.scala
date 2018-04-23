package com.igeolise.chrome_headless_crawler

import java.io.File
import java.nio.file.Files
import java.util.Base64

import com.igeolise.chrome_headless_crawler.command_parser.Credentials
import com.igeolise.chrome_headless_crawler.model.LogEntry
import io.webfolder.cdp.`type`.constant.DownloadBehavior
import io.webfolder.cdp.command.DOM
import io.webfolder.cdp.event.Events
import io.webfolder.cdp.event.network.LoadingFinished
import io.webfolder.cdp.event.page.LifecycleEvent
import io.webfolder.cdp.listener.EventListener
import io.webfolder.cdp.session.Session

import scala.collection.JavaConverters._
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Await, Promise}
import scalaz.\/
import scalaz.syntax.std.option._

object SessionHelpers {

  implicit class SessionHelpersExt(val session: Session) extends AnyVal {

    def setupDownloadBehaviour(target: File): LogEntry \/ Unit = {
      \/.fromTryCatchNonFatal {
        val page = session.getCommand.getPage
        page.setDownloadBehavior(DownloadBehavior.Allow, target.getAbsolutePath)
      }.leftMap(e => LogEntry(s"Failed to set download behaviour\nException message: ${e.getMessage}"))
    }

    def setCredentials(credentials: Credentials): LogEntry \/ Unit = {
      \/.fromTryCatchNonFatal {
        val network = session.getCommand.getNetwork
        network.enable()
        val headers: java.util.Map[String, Object] = Map[String, Object]("Authorization" -> ("Basic " + new String(Base64.getEncoder.encode(s"${credentials.user}:${credentials.password}".getBytes()))).asInstanceOf[Object]).asJava
        network.setExtraHTTPHeaders(headers)
      }.leftMap(e => LogEntry(s"Failed to set credentials with message: ${e.getMessage}"))
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

    def getDocumentNodeId(): LogEntry \/ Int = \/.fromTryCatchNonFatal(
      session.getDom().getDocument.getNodeId.toInt
    ).leftMap(e => LogEntry(s"Failed to get document node id\nException message: ${e.getMessage}"))

    def waitForPage(action: (Session) => Unit, timeout: FiniteDuration): LogEntry \/ Unit = {
      import  scala.concurrent.duration._

      val promise = Promise[Unit]()
      val listener = new EventListener[AnyRef] {
        override def onEvent(event: Events, value: scala.AnyRef): Unit = {
          value match {
            case _: LoadingFinished => promise.success(())
            case e: LifecycleEvent if e.getName == "networkIdle" => promise.success(())
            case _ =>
          }
        }
      }
      session.addEventListener(listener)
      action(session)
      val future = promise.future
      Await.ready(future, atMost = timeout)
      val result = future.value.map(_ => ()).toRightDisjunction(LogEntry("Browser waiting for action to complete timeout"))
      session.waitDocumentReady()
      session.removeEventEventListener(listener)
      result
    }

    def getNodeAttributes(nodeId: Int): LogEntry \/ Map[String, String]  = {
      \/.fromTryCatchNonFatal {
        val attributeList = session.getDom().getAttributes(nodeId).asScala.toList
        attributeList.grouped(2).map { l => l.head -> l.tail.head }.toMap
      }.leftMap(e => LogEntry(s"Failed to get node attributes\nException message: ${e.getMessage}"))
    }

    private def createTempDir(prefix: String): LogEntry \/ File = {
      \/.fromTryCatchNonFatal(Files.createTempDirectory(prefix).toFile)
        .leftMap(e => LogEntry(s"Failed to create dir with message: ${e.getMessage}"))
    }

    private def moveFiles(from: File, to: File): LogEntry \/ Unit = {
      \/.fromTryCatchNonFatal(Files.move(from.toPath, to.toPath)).map(_ => ())
        .leftMap(e => LogEntry(s"Failed while moving file from: ${from.getAbsolutePath} to: ${to.getAbsolutePath}\nException message: ${e.getMessage}"))
    }

    def download(action: (Session) => Unit, target: File, timeout: FiniteDuration): LogEntry \/ File = {
      for {
        tempDir <- createTempDir("chr_crawler")
        _       <- setupDownloadBehaviour(tempDir)
        _       <- session.waitForPage(action, timeout)
        downloadedFile  <- tempDir.listFiles().headOption.toRightDisjunction(LogEntry("Downloaded file not found"))
        resultFile      <- \/.fromTryCatchNonFatal(new File(target, downloadedFile.getName))
                              .leftMap(_ => LogEntry(s"Failed to create file ${downloadedFile.getName} in ${target.getAbsolutePath}"))
        _       <- moveFiles(downloadedFile, resultFile)
      } yield resultFile
    }

    def focus(id: Int): LogEntry \/ Session = {
      \/.fromTryCatchNonFatal {
        session.getCommand.getDOM.focus(id, null, null)
        session
      }.leftMap(e => LogEntry(s"Failed to focus on element $id\nException message: ${e.getMessage}"))
    }

    def typeIn(text: String): LogEntry \/ Session = {
      \/.fromTryCatchNonFatal(
        session.sendKeys(text)
      ).leftMap(e => LogEntry(s"Failed to enter text: '$text'\nException message: ${e.getMessage}"))
    }
  }

}
