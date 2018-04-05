package com.igeolise.chrome_headless_crawler.model

import scalaz.\/
import scalaz.syntax.std.option._

case class ElementStack(elements: List[Int]) {
  def push(elementId: Int): ElementStack = {
    this.copy(elements = elementId +: elements)
  }
  def pop: LogEntry \/ (Int, ElementStack) = {
    elements.headOption.map(e => (e, this.copy(elements = elements.tail))).toRightDisjunction(LogEntry("Element stack is empty"))
  }
}
