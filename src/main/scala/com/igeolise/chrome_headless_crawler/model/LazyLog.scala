package com.igeolise.chrome_headless_crawler.model

case class LogEntry(message: String)

case class LazyLog(header: String, log: Seq[LogEntry] = Seq.empty) {

  def append(message: LogEntry): LazyLog = {
    this.copy(log = message +: log)
  }
  def success: String = header + " succeeded"

  def getMessageString: String = (header +: log).mkString("\n")

}
