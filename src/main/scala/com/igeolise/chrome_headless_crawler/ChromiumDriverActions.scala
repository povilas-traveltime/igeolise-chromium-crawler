package com.igeolise.chrome_headless_crawler

import com.igeolise.Helpers.TryOps
import org.openqa.selenium.WebElement
import org.openqa.selenium.chrome.ChromeDriver

import scala.util.Try

trait ChromiumDriverActions {

  import scala.collection.JavaConverters._

  val driver: ChromeDriver

  def getElement(selector: String): Either[String, WebElement] = Try(driver.findElementByXPath(selector)).leftMap(e => s"Could not select element with selector '$selector', cause: ${e.getMessage}")

  def getElements(selector: String): Either[String, List[WebElement]] = Try(driver.findElementsByXPath(selector).asScala.toList).leftMap(e => s"Could not select element with selector '$selector', cause: ${e.getMessage}")

  def getDocumentRootNode: Either[String, WebElement] = Try(driver.findElementByCssSelector("body")).leftMap(e => s"Could not select root element, cause: ${e.getMessage}")
}