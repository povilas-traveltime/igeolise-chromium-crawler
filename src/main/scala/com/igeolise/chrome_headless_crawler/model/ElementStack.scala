package com.igeolise.chrome_headless_crawler.model

import org.openqa.selenium.WebElement
import scalaz.\/
import scalaz.syntax.std.option._

case class ElementStack(elements: List[WebElement]) {
  def push(element: WebElement): ElementStack = {
    this.copy(elements = element +: elements)
  }
  def pop: LogEntry \/ (WebElement, ElementStack) = {
    elements.headOption.map(e => (e, this.copy(elements = elements.tail))).toRightDisjunction(LogEntry("Element stack is empty"))
  }
}
