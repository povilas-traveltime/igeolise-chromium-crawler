package com.igeolise.chrome_headless_crawler.model

import monocle.macros.GenLens

object ComposedLenses {
  val scriptStateL = GenLens[CrawlerState](_.scriptState)
  val scriptL = GenLens[ScriptState](_.script)
  val currentScriptL = scriptStateL composeLens scriptL
  val stackL = GenLens[ScriptState](_.elementStack)
  val elementStackL = scriptStateL composeLens stackL
  val logL = GenLens[ScriptState](_.log)
  val scriptLogL = scriptStateL composeLens logL
}
