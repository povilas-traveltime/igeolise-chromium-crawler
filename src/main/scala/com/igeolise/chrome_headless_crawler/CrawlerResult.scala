package com.igeolise.chrome_headless_crawler

import java.io.File

object CrawlerResult {

  case class ScriptWithLog[A, B](script: B, log: A)
  case class FileWithLog[A](file: File, log: A)

  sealed abstract class CrawlerSuccess

  case class Ok[A](downloaded: List[FileWithLog[A]]) extends CrawlerSuccess
  case class WithFailures[A, B](downloaded: List[FileWithLog[A]], failed: List[ScriptWithLog[A, B]]) extends CrawlerSuccess

  case class CrawlerFailure(message: String)
}
