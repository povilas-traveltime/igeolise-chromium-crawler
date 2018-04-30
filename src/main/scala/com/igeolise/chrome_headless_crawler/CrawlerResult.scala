package com.igeolise.chrome_headless_crawler

import java.io.File

object CrawlerResult {

  case class ScriptWithLog[A, B](script: B, log: A)
  case class FileWithLog[A](file: File, log: A)

  sealed trait CrawlerResult

  case class CrawlerResults[A, B](downloaded: List[FileWithLog[A]], failed: List[ScriptWithLog[A, B]]) extends CrawlerResult

  case class CrawlerFailure(message: String) extends CrawlerResult
}
