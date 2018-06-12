package com.igeolise.chrome_headless_crawler

import org.openqa.selenium.By.ByXPath
import org.openqa.selenium.{JavascriptExecutor, Keys, WebDriver, WebElement}
import scalaz.{-\/, \/, \/-}

import scala.collection.JavaConverters._

object WebElementExtensions {
  implicit class WebElementExt(val element: WebElement) extends AnyVal {
    def clickDisjunction(driver: WebDriver): String \/ Unit = \/.fromTryCatchNonFatal {
      if (element.getAttribute("target") != "_self")
        driver.asInstanceOf[JavascriptExecutor].executeScript("arguments[0].removeAttribute('target')", element)
      element.click()
  }.leftMap(e => s"Unable to click on element, cause: ${e.getMessage}")

    def findElementsByXpath(xpath: String): String \/ List[WebElement] = \/.fromTryCatchNonFatal(element.findElements(new ByXPath(xpath))) match {
      case \/-(elementList) =>
        val scalaList = elementList.asScala
        if (scalaList.nonEmpty) \/-(scalaList.toList)
        else -\/(s"Finding elements by xpath $xpath returned empty list")
      case -\/(e) => -\/(s"Failed to find elements, cause: ${e.getMessage}")
    }
    def findElementByXpath(xpath: String): String \/ WebElement = \/.fromTryCatchNonFatal(
      element.findElement(new ByXPath(xpath))
    ).leftMap(e => s"Failed to get element, cause: ${e.getMessage}")
  }
}