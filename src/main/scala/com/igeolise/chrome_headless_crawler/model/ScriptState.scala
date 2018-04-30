package com.igeolise.chrome_headless_crawler.model

import monocle.macros.Lenses

@Lenses
case class ScriptState(script: Script, elementStack: ElementStack, log: LazyLog)
