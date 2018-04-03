package com.igeolise.chrome_headless_crawler

import java.io.File

import io.webfolder.cdp.session.Session
import CrawlerResult._
import scala.concurrent.duration.FiniteDuration

case class CrawlerState(
  elementStack:   Seq[Int],
  script:         Script,
  lazyLogger:     LazyLog,
  target:         File,
  session:        Session,
  successes:      List[FileWithLog[LazyLog]],
  failures:       List[ScriptWithLog[LazyLog, Script]],
  unprocessedScripts: List[Script],
  timeout:        FiniteDuration
) {
  def getStackTop: Int = elementStack.headOption.getOrElse(throw new Exception("Element stack empty"))
  def appendStack(id: Int): CrawlerState = this.copy(id +: elementStack)
  def stackTail: CrawlerState = this.copy(elementStack.tail)
  def stateWithNextAction: Option[CrawlerState] = {
    this.script.withNextAction.map(s => this.copy(script = s))
  }
}

object CrawlerState {
  def createState(session: Session, script: Script, downloadTarget: File, timeout: FiniteDuration): CrawlerState = {
    CrawlerState(
      Seq.empty, script, LazyLog("Crawling " + script.toString),
      downloadTarget, session, List.empty, List.empty, List.empty, timeout
    )
  }
}