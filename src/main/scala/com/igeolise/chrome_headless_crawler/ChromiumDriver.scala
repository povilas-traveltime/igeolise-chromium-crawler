package com.igeolise.chrome_headless_crawler

import java.io.File
import java.net.URL
import java.nio.file.{Files, Path}
import java.util.logging.Level

import com.igeolise.chrome_headless_crawler.model.LogEntry
import com.igeolise.geoliser.utils.Resourceful.ResourceScope
import org.apache.http.client.methods.{CloseableHttpResponse, HttpPost}
import org.apache.http.client.utils.URIBuilder
import org.apache.http.entity.{ContentType, StringEntity}
import org.apache.http.impl.client.HttpClientBuilder
import org.openqa.selenium.chrome.{ChromeDriver, ChromeDriverService, ChromeOptions}
import org.openqa.selenium.logging.{LogType, LoggingPreferences, LogEntry => SLogEntry}
import org.openqa.selenium.remote.CapabilityType
import scalaz.{-\/, \/, \/-}
import scalaz.syntax.id._

import scala.collection.mutable
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.FiniteDuration
import scala.util.Try
import com.igeolise.geoliser.utils.Resourceful.acquire
import com.igeolise.geoliser.utils.TempDirResource
import play.api.libs.json.Json

import scala.concurrent.ExecutionContext.Implicits.global

class ChromiumDriver(chromeOptions: ChromeOptions = ChromiumDriver.defaultOptions)
  extends AutoCloseable with ChromiumDriverActions {

  val (service, driver) = createDriver(chromeOptions)

  override def close(): Unit = {
    driver.close()
  }

  private def createDriver(options: ChromeOptions): (ChromeDriverService, ChromeDriver) = {
    val driverService = ChromeDriverService.createDefaultService()
    val driver = new ChromeDriver(driverService, options)
    (driverService, driver)
  }

  private def buildSendCommandUrl(chromiumUrl: String, sessionId: String): String \/ URL = \/.fromTryCatchNonFatal {
    val builder = new URIBuilder(chromiumUrl)
    builder.setPath("session/" + sessionId + "/chromium/send_command")
    builder.build().toURL
  }.leftMap(_ => s"Could not build url for session send_command")

  private def checkDirectoryExists(dir: File): String \/ File = {
    if (dir.exists() && dir.isDirectory) \/-(dir)
    else -\/(s"${dir.getAbsolutePath} does not exist or is not a directory")
  }

  private def createJsonRequest(url: URL, content: String) = \/.fromTryCatchNonFatal {
    val request = new HttpPost(url.toURI)
    request.setHeader("Content-Type", ContentType.APPLICATION_JSON.getMimeType)
    request.setEntity(new StringEntity(content, ContentType.APPLICATION_JSON))
    request
  }.leftMap(_ => "Could not create POST JSON request")

  private def checkResponseStatus(response: CloseableHttpResponse): String \/ Unit = {
    if (response.getStatusLine.getStatusCode == 200) \/-(())
    else -\/(s"Received response failure reason: ${response.getStatusLine.getReasonPhrase}")
  }

  private def setDownloadBehaviour(service: ChromeDriverService, driver: ChromeDriver, downloadPath: File): String \/ Unit = {
    val command =
      s"""
        {"cmd":"Page.setDownloadBehavior","params":{"behavior":"allow","downloadPath": "${downloadPath.getAbsolutePath}"}}
      """.stripMargin

    ResourceScope { implicit scope =>
      for {
        downloadTarget  <- checkDirectoryExists(downloadPath)
        chromiumUrl     <- \/.fromTryCatchNonFatal(service.getUrl.toString).leftMap(_ => "Could not get Chromium service url")
        sessionId       <- \/.fromTryCatchNonFatal(driver.getSessionId.toString).leftMap(_ => "Could not get Chromium session id")
        url             <- buildSendCommandUrl(chromiumUrl, sessionId)
        request         <- createJsonRequest(url, command)
        httpClient      <- \/.fromTryCatchNonFatal(acquire(HttpClientBuilder.create().build())).leftMap(_ => "Could not create HttpClient")
        response        <- \/.fromTryCatchNonFatal(acquire(httpClient.execute(request))).leftMap(_ => s"Could not execute post request to ${url.toString}")
        responseStatus  <- checkResponseStatus(response)
      } yield responseStatus
    }
  }

  protected def setDownloadFolder(folder: File): String \/ Unit = setDownloadBehaviour(service, driver, folder)

  def download(work: () => \/[String, Unit], target: File) (implicit timeout: FiniteDuration): String \/ List[File] = {
    ResourceScope { implicit scope =>
      for {
        tempDir     <- \/.fromTryCatchNonFatal(acquire(new TempDirResource("chr_crawler")).tempDir.toFile).leftMap(e => s"Could not create temp directory, cause: ${e.getMessage}")
        _           <- setDownloadFolder(tempDir)
        downloaded  <- waitForDownloadComplete(work, tempDir, timeout)
        result      <- moveFiles(downloaded, target)
      } yield result
    }
  }

  private def moveFiles(files: List[File], target: File): String \/ List[File] = {
    \/.fromTryCatchNonFatal(files.map { f =>
      val newFile = new File(target, f.getName)
      moveFile(f, newFile)
      newFile
    }).leftMap(e => s"Could not move files to ${target.getAbsolutePath}, cause: ${e.getMessage}")
  }

  private def moveFile(from: File, to: File): LogEntry \/ Unit = {
    \/.fromTryCatchNonFatal(Files.move(from.toPath, to.toPath)).map(_ => ())
      .leftMap(e => LogEntry(s"Failed while moving file from: ${from.getAbsolutePath} to: ${to.getAbsolutePath}\nException message: ${e.getMessage}"))
  }

  def navigateTo(url: String) (implicit timeout: FiniteDuration): String \/ Unit = {
    waitForPage(
      () => \/.fromTryCatchNonFatal(driver.get(url)).leftMap(e => s"Failed to navigate to $url, cause: ${e.getMessage}")
    )
  }

  import scala.collection.JavaConverters._
  def waitForPage(work: () => String \/ Unit) (implicit timeout: FiniteDuration): String \/ Unit = {
    println("waitingForPage")
    val doneWork = work()
    val future = Future {
      def logs = driver.manage().logs().get(LogType.PERFORMANCE)
      def checkLoadingDone(allMessages: Seq[SLogEntry]) = {
        val messages = allMessages.takeRight(3).map(l => Json.parse(l.getMessage))
        println(messages.map(_("message")("method").toString()).mkString("\n\t"))
        val cond = messages.exists(_("message")("method").toString().contains("Page.frameStoppedLoading"))
        println(cond)
        cond
      }
      do {
        Thread.sleep(100)
      } while (!checkLoadingDone(logs.getAll.asScala))
    }
    for {
      result <- work()
      _ <- \/.fromTryCatchNonFatal(Await.ready(future, timeout)).leftMap(_ => "Timeout while waiting for action to complete")
    } yield result
  }

  private def waitForDownloadComplete(work: () => \/[String, Unit], target: File, timeout: FiniteDuration): String \/ List[File]  = {
    val filesBefore = target.listFiles().toSet
    def diff = target.listFiles().toSet.diff(filesBefore)
    def checkNoNewFiles = diff.size < 1
    def checkStillDownloading = diff.exists(_.getName.endsWith(".crdownload"))
    def checkExistsZip = diff.exists(_.getName.endsWith(".zip"))

    val future = Future {
      do {
        Thread.sleep(100)
      } while (checkNoNewFiles || (!checkExistsZip && checkStillDownloading))
    }
    val doneWork = work()
    val finished = \/.fromTryCatchNonFatal(Await.ready(future, timeout))
    (doneWork, diff.nonEmpty, finished) match {
      case (-\/(message), _, _) => -\/(s"Failed to perform download action: cause ${message}")
      case (\/-(_), true, \/-(_)) => \/-(diff.toList)
      case (\/-(_), _, -\/(_)) => -\/("Timeout while download")
      case (\/-(_), false, _) => -\/("No files were downloaded")
    }
  }
}

object ChromiumDriver {

  def defaultOptions: ChromeOptions = createOptions |> setDownloadPreferences |> setHeadlessOptions |> enableLogging

  def createOptions = new ChromeOptions

  def enableLogging(options: ChromeOptions): ChromeOptions = {
    val loggingPreferences = new LoggingPreferences()
    loggingPreferences.enable(LogType.PERFORMANCE, Level.ALL)
    options.setCapability(CapabilityType.LOGGING_PREFS, loggingPreferences)
    options
  }

  def setHeadlessOptions(options: ChromeOptions): ChromeOptions = {
    options.setHeadless(true)
    options.addArguments("--test-type", "--disable-extensions")
    options
  }

  def setDownloadPreferences(options: ChromeOptions): ChromeOptions = {
    val prefs = new mutable.HashMap[String, Object]()
    prefs.put("download.prompt_for_download", false.asInstanceOf[Object])
    prefs.put("download.directory_upgrade", true.asInstanceOf[Object])
    options.setExperimentalOption("prefs", prefs)
  }

  def withDriver[A](work: ChromiumDriver => A): Try[A] = {
    Try { ResourceScope { implicit scope =>
      work(acquire(new ChromiumDriver()))
    } }
  }
}