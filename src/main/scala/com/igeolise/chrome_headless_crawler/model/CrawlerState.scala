package com.igeolise.chrome_headless_crawler.model

import java.io.File

import com.igeolise.chrome_headless_crawler.CrawlerResult.{FileWithLog, ScriptWithLog}
import io.webfolder.cdp.session.Session
import monocle.macros.Lenses

import scala.concurrent.duration.FiniteDuration

@Lenses
case class CrawlerState(
  scriptState:    ScriptState,
  target:         File,
  session:        Session,
  successes:      List[FileWithLog[LazyLog]],
  failures:       List[ScriptWithLog[LazyLog, Script]],
  unprocessedScripts: List[Script],
  timeout:        FiniteDuration
)

object CrawlerStateFactory {
  def createState(session: Session, script: Script, downloadTarget: File, timeout: FiniteDuration): CrawlerState = {
    CrawlerState(
      ScriptState(script, ElementStack(List.empty), LazyLog("Crawling " + script.toString)),
      downloadTarget, session, List.empty, List.empty, List.empty, timeout
    )
  }
}