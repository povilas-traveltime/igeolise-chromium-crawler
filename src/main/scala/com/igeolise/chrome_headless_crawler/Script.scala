package com.igeolise.chrome_headless_crawler

import com.igeolise.chrome_headless_crawler.command_parser.Action

case class Script(actions: Vector[Action], currentActionIdx: Int) {
  def getCurrentAction: Action = actions(currentActionIdx)
  def hasNext: Boolean = actions.length <= currentActionIdx
  def withNextAction: Option[Script] = {
    if (hasNext) Some(this.copy(currentActionIdx = currentActionIdx + 1))
    else None
  }
  def replaceCurrentActionAndReset(newAction: Action): Script = {
    val done = actions.slice(0, currentActionIdx)
    val left = actions.slice(currentActionIdx + 1, actions.length)
    Script((done :+ newAction) ++ left, 0)
  }

  override def toString: String = actions.mkString("\\")
}
