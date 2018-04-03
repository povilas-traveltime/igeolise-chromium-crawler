package com.igeolise.chrome_headless_crawler

case class LazyLog(header: String, log: Seq[String] = Seq.empty) {

  def info(msg: String): LazyLog = {
    this.copy(log = msg +: log)
  }
  def success: String = header + " succeeded"

  def failure(msg: String) : Seq[String] = header +: log :+ msg

}
