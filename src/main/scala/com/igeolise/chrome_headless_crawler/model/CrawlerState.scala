package com.igeolise.chrome_headless_crawler.model

import java.io.File

import com.igeolise.chrome_headless_crawler.ChromiumDriver
import com.igeolise.chrome_headless_crawler.CrawlerResult.{FileWithLog, ScriptWithLog}

import scala.concurrent.duration.FiniteDuration

case class CrawlerState(
  scriptState:    ScriptState,
  target:         File,
  driver:         ChromiumDriver,
  successes:      List[FileWithLog[LazyLog]],
  failures:       List[ScriptWithLog[LazyLog, Script]],
  unprocessedScripts: List[Script],
  timeout:        FiniteDuration
)

object CrawlerStateFactory {
  def createState(driver: ChromiumDriver, script: Script, downloadTarget: File, timeout: FiniteDuration): CrawlerState = {
    CrawlerState(
      ScriptState(script, ElementStack(List.empty), LazyLog("Crawling " + script.toString)),
      downloadTarget, driver, List.empty, List.empty, List.empty, timeout
    )
  }
}