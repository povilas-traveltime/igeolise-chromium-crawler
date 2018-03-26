package com.igeolise.chrome_headless_crawler

import java.io.File

import com.igeolise.chrome_headless_crawler.command_parser.Action
import io.webfolder.cdp.session.Session

import scala.concurrent.duration.FiniteDuration

case class CrawlerState(
  elementStack:   Seq[Int],
  doneActions:    Seq[Action],
  pendingActions: Seq[Action],
  lazyLogger:     LazyLog,
  target:         File,
  session:        Session,
  successes:      Seq[File],
  failures:       Seq[Seq[Action]],
  unprocessedScripts: Seq[Seq[Action]],
  timeout:        FiniteDuration
) {
  def getStackTop: Int = elementStack.headOption.getOrElse(throw CrawlerException("Element stack empty"))
  def appendStack(id: Int): CrawlerState = this.copy(id +: elementStack)
  def stackTail: CrawlerState = this.copy(elementStack.tail)
  def moveActionToDone: CrawlerState = {
    pendingActions match {
      case a :: tail => this.copy(doneActions = this.doneActions :+ a, pendingActions = tail)
      case _ => this
    }

  }
}

object CrawlerState {
  def createState(session: Session, actionSequence: Seq[Action], downloadTarget: File, timeout: FiniteDuration): CrawlerState = {
    CrawlerState(
      Seq.empty, Seq.empty, Seq.empty, LazyLog("Crawling " + actionSequence.mkString("|")),
      downloadTarget, session, Seq.empty, Seq.empty, Seq(actionSequence), timeout
    )
  }
}