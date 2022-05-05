package com.igeolise.chrome_headless_crawler.model

import com.softwaremill.quicklens._

object ComposedLenses {
  val scriptStateL = modify[CrawlerState](_.scriptState)
  val scriptL = modify[ScriptState](_.script)
  val currentScriptL = scriptStateL andThenModify scriptL
  val stackL = modify[ScriptState](_.elementStack)
  val elementStackL = scriptStateL andThenModify stackL
  val logL = modify[ScriptState](_.log)
  val scriptLogL = scriptStateL andThenModify  logL
}
