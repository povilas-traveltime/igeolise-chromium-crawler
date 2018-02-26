package com.igeolise.chrome_headless_crawler

import java.io.File

import crawler.command_parser.Action
import io.webfolder.cdp.session.Session

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
  timeout:        Int
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