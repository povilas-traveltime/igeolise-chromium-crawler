package com.igeolise.chrome_headless_crawler

import java.net.ServerSocket
import io.webfolder.cdp.Launcher
import io.webfolder.cdp.session.{Session, SessionFactory}
import scala.collection.JavaConversions._
import scala.concurrent._
import scala.util.Try
import scala.util.control.NonFatal
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._


class ChromeSession(chromeLaunchCommand: String, sessionFactoryParams: Seq[String]) {

  private def waitPortClosed(port: Int): Unit = {
    try {
      new ServerSocket(port).close()
    } catch { case NonFatal(_) =>
      wait(1000)
      waitPortClosed(port)
    }
  }

  private def destroyFactory(factory: SessionFactory): Unit = {
    val port = factory.getPort
    factory.close()
    val f = Future(waitPortClosed(port))
    Await.ready(f, 5.minutes)
  }

  private def getFreePort: Try[Int] = Try {
    val s = new ServerSocket(0)
    val port= s.getLocalPort
    s.close()
    port
  }

  private def createShutdownHook(factory: SessionFactory): Unit = {
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        destroyFactory(factory)
      }
    })
  }

  def withSession[T](f :(Session) => T): Try[T] = {
    getFreePort.map { p =>
      val launcher = new Launcher(p)
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
}

object ChromeSession {
  val defaultArguments = List("--headless", "--disable-gpu", "--window-size=1280,1024", "--no-sandbox")
  val defaultStartCommand = "chromium-browser"
}