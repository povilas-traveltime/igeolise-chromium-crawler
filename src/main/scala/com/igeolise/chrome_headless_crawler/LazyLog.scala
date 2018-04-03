package com.igeolise.chrome_headless_crawler

case class LogEntry(message: String)

case class LazyLog(header: String, log: Seq[LogEntry] = Seq.empty) {

  def info(message: LogEntry): LazyLog = {
    this.copy(log = message +: log)
  }
  def success: String = header + " succeeded"

  def failure(message: LogEntry) : Seq[String] = (header +: log :+ message).map(_.toString)

}
