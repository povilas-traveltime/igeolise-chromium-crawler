package com.igeolise.chrome_headless_crawler

import org.openqa.selenium.WebElement
import org.openqa.selenium.chrome.ChromeDriver
import scalaz.\/

trait ChromiumDriverActions {

  import scala.collection.JavaConverters._

  val driver: ChromeDriver

  def getElement(selector: String): String \/ WebElement = \/.fromTryCatchNonFatal(driver.findElementByCssSelector(selector)).leftMap(e => s"Could not select element with selector '$selector', cause: ${e.getMessage}")

  def getElements(selector: String): String \/ List[WebElement] = \/.fromTryCatchNonFatal(driver.findElementsByCssSelector(selector).asScala.toList).leftMap(e => s"Could not select element with selector '$selector', cause: ${e.getMessage}")

  def getDocumentRootNode: String \/ WebElement = \/.fromTryCatchNonFatal(driver.findElementByXPath("/*")).leftMap(e => s"Could not select root element, cause: ${e.getMessage}")
}