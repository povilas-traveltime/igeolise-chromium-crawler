package com.igeolise.chrome_headless_crawler

import java.io.IOException
import java.net.ServerSocket

import io.webfolder.cdp.Launcher
import io.webfolder.cdp.session.{Session, SessionFactory}
import scala.collection.JavaConversions._
import sys.process._


class ChromeSession(chromeLaunchCommand: String, sessionFactoryParams: Seq[String]) {

  val portScanFloor = 9500
  val portScanCeiling = 60000

  private def destroyFactory(factory: SessionFactory): Unit = {
    val port = factory.getPort
    factory.close()
    s"netstat -antu | kill -9 $$( lsof -i:$port -t)" !
  }

  private def getFreePort: Int = {
    (portScanFloor to portScanCeiling).find { port =>
      try {
        new ServerSocket(port).close()
        true
      }
      catch { case e: IOException => false}
    }.getOrElse(throw new Exception("no free port found"))
  }

  private def createShutdownHook(factory: SessionFactory): Unit = {
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        destroyFactory(factory)
      }
    })
  }

  def withSession[T](f :(Session) => T): T = {
    val launcher = new Launcher(getFreePort)
    val factory = launcher.launch(chromeLaunchCommand, sessionFactoryParams)
    createShutdownHook(factory)
    val session = factory.create()
    val result = f(session)
    session.close()
    factory.close()
    destroyFactory(factory)
    result
  }
}

object ChromeSession {
  val defaultArguments = List("--headless", "--disable-gpu", "--window-size=1280,1024", "--no-sandbox")
  val defaultStartCommand = "chromium-browser"
}