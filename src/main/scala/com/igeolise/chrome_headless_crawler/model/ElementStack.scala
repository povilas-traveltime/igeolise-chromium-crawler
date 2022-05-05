package com.igeolise.chrome_headless_crawler.model

import org.openqa.selenium.WebElement

case class ElementStack(elements: List[WebElement]) {
  def push(element: WebElement): ElementStack = {
    this.copy(elements = element +: elements)
  }
  def pop: Either[LogEntry, (WebElement, ElementStack)] = {
    elements.headOption.map(e => (e, this.copy(elements = elements.tail))).toRight(LogEntry("Element stack is empty"))
  }
}
