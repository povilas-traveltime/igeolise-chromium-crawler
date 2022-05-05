package com.igeolise.chrome_headless_crawler

import com.igeolise.Helpers._
import org.openqa.selenium.By.ByXPath
import org.openqa.selenium.{JavascriptExecutor, Keys, WebDriver, WebElement}

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

object WebElementExtensions {
  implicit class WebElementExt(val element: WebElement) extends AnyVal {
    def clickEither(driver: WebDriver): Either[String, Unit] = Try {
      if (element.getAttribute("target") != "_self")
        driver.asInstanceOf[JavascriptExecutor].executeScript("arguments[0].removeAttribute('target')", element)
      element.click()
  }.leftMap(e => s"Unable to click on element, cause: ${e.getMessage}")

    def findElementsByXpath(xpath: String): Either[String, List[WebElement]] = Try(element.findElements(new ByXPath(xpath))) match {
      case Success(elementList) =>
        val scalaList = elementList.asScala
        if (scalaList.nonEmpty) Right(scalaList.toList)
        else Left(s"Finding elements by xpath $xpath returned empty list")
      case Failure(e) => Left(s"Failed to find elements, cause: ${e.getMessage}")
    }
    def findElementByXpath(xpath: String): Either[String, WebElement] = Try (
      element.findElement(new ByXPath(xpath))
    ).leftMap(e => s"Failed to get element, cause: ${e.getMessage}")
  }
}