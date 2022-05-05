package com.igeolise.chrome_headless_crawler

import java.io.File
import java.net.URL
import java.nio.file.{Files, StandardCopyOption}
import java.util
import java.util.logging.Level

import com.igeolise.Helpers.{IdOps, JsValueOps, TryOps}
import com.igeolise.chrome_headless_crawler.ChromiumDriver.ChromiumDriverParams
import com.igeolise.geoliser.utils.Resourceful.ResourceScope
import org.apache.http.client.methods.{CloseableHttpResponse, HttpPost}
import org.apache.http.client.utils.URIBuilder
import org.apache.http.entity.{ContentType, StringEntity}
import org.apache.http.impl.client.HttpClientBuilder
import org.openqa.selenium.chrome.{ChromeDriver, ChromeDriverService, ChromeOptions}
import org.openqa.selenium.logging.{LogType, LoggingPreferences, Logs, LogEntry => SLogEntry}
import org.openqa.selenium.remote.CapabilityType

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.FiniteDuration
import scala.util.Try
import com.igeolise.geoliser.utils.Resourceful.acquire
import com.igeolise.geoliser.utils.TempDirResource
import play.api.libs.json.{JsString, JsValue, Json}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent._
import ExecutionContext.Implicits.global


class ChromiumDriver(params: ChromiumDriverParams)
  extends AutoCloseable with ChromiumDriverActions {

  System.setProperty("webdriver.chrome.logFile", params.logFile.getAbsolutePath)
  val service: ChromeDriverService = params.service
  val driver: ChromeDriver = new ChromeDriver(service, params.options)

  println(driver.manage().logs().getAvailableLogTypes)

  override def close(): Unit = {
    driver.quit()
    service.stop()
  }

  private def buildSendCommandUrl(chromiumUrl: String, sessionId: String): Either[String, URL] = Try {
    val builder = new URIBuilder(chromiumUrl)
    builder.setPath("session/" + sessionId + "/chromium/send_command")
    builder.build().toURL
  }.leftMap(_ => s"Could not build url for session send_command")

  private def checkDirectoryExists(dir: File): Either[String, File] = {
    if (dir.exists() && dir.isDirectory) Right(dir)
    else Left(s"${dir.getAbsolutePath} does not exist or is not a directory")
  }

  private def createJsonRequest(url: URL, content: String) = Try {
    val request = new HttpPost(url.toURI)
    request.setHeader("Content-Type", ContentType.APPLICATION_JSON.getMimeType)
    request.setEntity(new StringEntity(content, ContentType.APPLICATION_JSON))
    request
  }.leftMap(_ => "Could not create POST JSON request")

  private def checkResponseStatus(response: CloseableHttpResponse): Either[String, Unit] = {
    if (response.getStatusLine.getStatusCode == 200) Right(())
    else Left(s"Received response failure reason: ${response.getStatusLine.getReasonPhrase}")
  }

  private def setDownloadBehaviour(service: ChromeDriverService, driver: ChromeDriver, downloadPath: File): Either[String, Unit] = {
    val command =
      s"""
        {"cmd":"Page.setDownloadBehavior","params":{"behavior":"allow","downloadPath": "${downloadPath.getAbsolutePath}"}}
      """.stripMargin

    ResourceScope { implicit scope =>
      for {
        _               <- checkDirectoryExists(downloadPath)
        chromiumUrl     <- Try(service.getUrl.toString).leftMap(_ => "Could not get Chromium service url")
        sessionId       <- Try(driver.getSessionId.toString).leftMap(_ => "Could not get Chromium session id")
        url             <- buildSendCommandUrl(chromiumUrl, sessionId)
        request         <- createJsonRequest(url, command)
        httpClient      <- Try(acquire(HttpClientBuilder.create().build())).leftMap(_ => "Could not create HttpClient")
        response        <- Try(acquire(httpClient.execute(request))).leftMap(_ => s"Could not execute post request to ${url.toString}")
        responseStatus  <- checkResponseStatus(response)
      } yield responseStatus
    }
  }

  protected def setDownloadFolder(folder: File): Either[String, Unit] = setDownloadBehaviour(service, driver, folder)

  def download(work: () => Either[String, Unit], target: File)(implicit timeout: FiniteDuration): Either[String, File] = {
    ResourceScope { implicit scope =>
      for {
        tempDir <- Try (acquire(new TempDirResource("chr_crawler")).tempDir.toFile).leftMap(e => s"Could not create temp directory, cause: ${e.getMessage}")
        _ <- setDownloadFolder(tempDir)
        downloaded <- waitForDownload(work, tempDir)
        result <- moveFile(downloaded, new File(target, downloaded.getName))
      } yield result
    }
  }

  private def moveFile(from: File, to: File): Either[String, File] = {
    Try(Files.move(from.toPath, to.toPath, StandardCopyOption.REPLACE_EXISTING).toFile)
      .leftMap(e => s"Failed while moving file from: ${from.getAbsolutePath} to: ${to.getAbsolutePath}\nException message: ${e.toString}")
  }

  def navigateTo(url: String)(implicit timeout: FiniteDuration): Either[String, Unit] = {
    waitForPage(
      () =>
        Try(driver.get(url)).leftMap(e => s"Failed to navigate to $url, cause: ${e.getMessage}")
    )
  }

  import scala.collection.JavaConverters._

  @tailrec
  private def waitAccumulator[A](waitTime: Long, acc: Iterable[A], nextVal: => Iterable[A], successCondition: Iterable[A] => Boolean): Iterable[A] = {
    val toEval = nextVal
    if (successCondition(toEval)) {
      acc ++ toEval
    }
    else {
      Thread.sleep(waitTime)
      waitAccumulator[A](waitTime, acc ++ toEval, nextVal, successCondition)
    }
  }

  private def waitAccumulator[A](condition: Iterable[A] => Boolean, init: Iterable[A], next: => Iterable[A]): Vector[A] = {
    val buffer = mutable.ArrayBuffer.empty[A]
    var current = init

    while (!condition(current) ) {
      buffer.appendAll(current)
      current = next
    }
    buffer.appendAll(current)
    buffer.toVector
  }

  def waitForDownload(work: () => Either[String, Unit], target: File)(implicit timeout: FiniteDuration): Either[String, File] = {

    val downloadIsFinished = Future {

      waitAccumulator(
        logContainsDownloadFinished,
        Seq.empty,
        (driver.manage().logs() |> { l: Logs =>
          l.get(LogType.PERFORMANCE).toJson.asScala
        })
      )
    }

    val workFuture = Future(work())(global)


    val jobFuture = for {
      _ <- workFuture
      logResult <- downloadIsFinished
    } yield (logResult)

    Try{Await.ready(jobFuture, timeout)}

    for {
      nonTimeoutLogs <- downloadIsFinished.value.toRight("Timed out while waiting for download.")
      nonFailedLogs <- nonTimeoutLogs.toEither.left.map(e => s"Exception thrown while downloading: ${e.getMessage}")
      fName <- getSuggestedFNameFromLog(nonFailedLogs).toRight("Could not find suggested file name for download.")
      file = new File(target, fName)
    } yield file

  }

  def getSuggestedFNameFromLog(logs: Iterable[SLogEntry]): Option[String] = {
    logs.find { log =>
      Try {
        val message = Json.parse(log.getMessage)
        message("message")("method").stringValue == "Page.downloadWillBegin" &&
          message("message")("params")("suggestedFilename").stringValue != ""
      }.getOrElse(false)
    }.map(l => Json.parse(l.getMessage)("message")("params")("suggestedFilename").stringValue)
  }

  private def logContainsTemplate(check: JsValue => Boolean)(logs: Iterable[SLogEntry]): Boolean = {
    logs.exists(l =>
      Try {
        val parsed = Json.parse(l.getMessage)
        check(parsed)
      }.getOrElse(false)
    )
  }

  private def logContainsPageLoadingFinished: Iterable[SLogEntry] => Boolean =
    logContainsTemplate { m =>
      m("message")("method").stringValue == "Page.frameStoppedLoading"
    }

  private def logContainsDownloadFinished: Iterable[SLogEntry] => Boolean =
    logContainsTemplate { m =>
      m("message") |> { mm =>
        mm("method").stringValue == "Page.downloadProgress" &&
          mm("params")("state").stringValue == "completed"
      }
    }

  def waitForPage(work: () => Either[String, Unit])(implicit timeout: FiniteDuration): Either[String, Unit] = {
    val doneWork = work()
    val future = Future {
      def logs = driver.manage().logs().get(LogType.PERFORMANCE)

      def checkLoadingDone(allMessages: Seq[SLogEntry]) = {
        allMessages
          .exists(l => l.getMessage.contains("Page.downloadProgress") && l.getMessage.contains("complete"))
      }

      do {
        Thread.sleep(1)
      } while (!checkLoadingDone(logs.toJson.asScala.toSeq))
    }(global)

    for {
      result <- doneWork
      _ <- Try(Await.ready(future, timeout)).leftMap(_ => "Timeout while waiting for action to complete")
    } yield result
  }
}

object ChromiumDriver {

  case class ChromiumDriverParams(
    chromeDriverFile: File,
    chromeFile: File,
    logFile: File,
    downloadDir: File
  ) {
    def options: ChromeOptions = {
      val loggingPreferences = new LoggingPreferences()
      loggingPreferences.enable(LogType.PERFORMANCE, Level.ALL)
      loggingPreferences.enable(LogType.BROWSER, Level.ALL)
      loggingPreferences.enable(LogType.DRIVER, Level.ALL)
      loggingPreferences.enable(LogType.CLIENT, Level.ALL)
      loggingPreferences.enable(LogType.PROFILER, Level.ALL)

      val prefs = new util.HashMap[String, Object]()
      prefs.put("download.prompt_for_download", false.asInstanceOf[Object])
      prefs.put("download.directory_upgrade", true.asInstanceOf[Object]  )
      prefs.put("download.default_directory", downloadDir.getAbsolutePath)

      val options = new ChromeOptions()
        .setBinary(chromeFile)
        .setHeadless(true)
        .addArguments(
          "--disable-extensions",
          "--no-sandbox",
          "--disable-popup-blocking",
          "--disable-dev-shm-usage",
          "--disable-gpu"
        )
        .setExperimentalOption("prefs", prefs)
        .setExperimentalOption("w3c", false)
      options.setCapability(CapabilityType.LOGGING_PREFS, loggingPreferences)
      options
    }

    def service: ChromeDriverService = {
      new ChromeDriverService
      .Builder()
        .withVerbose(true)
        .usingDriverExecutable(chromeDriverFile)
        .build()
    }
  }

  def withDriver[A](chromeDriverFile: File, chromeFile: File, logFile: File, downloadsDir: File)(work: ChromiumDriver => A): Try[A] = {
    Try { ResourceScope { implicit scope =>
      work(acquire(new ChromiumDriver(ChromiumDriverParams(chromeDriverFile, chromeFile, logFile, downloadsDir))))
    } }
  }
}