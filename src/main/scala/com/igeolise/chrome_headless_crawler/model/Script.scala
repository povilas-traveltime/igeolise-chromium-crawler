package com.igeolise.chrome_headless_crawler.model

import com.igeolise.chrome_headless_crawler.command_parser.Action
import monocle.macros.Lenses

import scala.util.Try

@Lenses
case class Script(actions: Vector[Action], currentActionIdx: Int) {
  def getAction: Option[Action] = {
    if (currentActionIdx >= 0 && currentActionIdx < actions.size) Some(actions(currentActionIdx))
    else None
  }
  def withNextAction: Script = {
    this.copy(currentActionIdx = currentActionIdx + 1)
  }
  def replaceCurrentActionAndReset(newAction: Action): Script = {
    val done = actions.slice(0, currentActionIdx)
    val left = actions.slice(currentActionIdx + 1, actions.length)
    Script((done :+ newAction) ++ left, 0)
  }
  def endScript: Script = this.copy(currentActionIdx = actions.length)

  override def toString: String = actions.mkString("\\")
}
