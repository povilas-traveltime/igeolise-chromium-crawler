package com.igeolise.chrome_headless_crawler

import org.openqa.selenium.WebElement
import scalaz.\/

object WebElementExtensions {
  implicit class WebElementExt(val element: WebElement) extends AnyVal {
    def clickDisjunction(): String \/ Unit = \/.fromTryCatchNonFatal(element.click()).leftMap(e => s"Unable to click on element, cause: ${e.getMessage}")
  }
}