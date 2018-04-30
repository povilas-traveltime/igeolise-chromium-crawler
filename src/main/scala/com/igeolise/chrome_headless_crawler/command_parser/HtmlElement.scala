package com.igeolise.chrome_headless_crawler.command_parser

sealed abstract class HtmlElement(val elementName: String) {
  val discriminator: Option[Discriminator]
  def discriminatorString: String = discriminator.map(d => s" having $d").getOrElse("")

  override def toString: String = elementName + discriminatorString
}

case class Form(discriminator: Option[Discriminator]) extends HtmlElement("form")
case class Input(discriminator: Option[Discriminator]) extends HtmlElement("input")
case class Anchor(discriminator: Option[Discriminator]) extends HtmlElement("anchor")
case class Div(discriminator: Option[Discriminator]) extends HtmlElement("div")
case class Span(discriminator: Option[Discriminator]) extends HtmlElement("span")
case class TableDataCell(discriminator: Option[Discriminator]) extends HtmlElement("td")
case class TableRow(discriminator: Option[Discriminator]) extends HtmlElement("tr")
case class Label(discriminator: Option[Discriminator]) extends HtmlElement("label")
case class AnyElement(discriminator: Option[Discriminator]) extends HtmlElement("anyElement")
case class Paragraph(discriminator: Option[Discriminator]) extends HtmlElement("p")
case class CustomSelector(selector: String) extends HtmlElement("customSelector") {
  val discriminator = None
  override def toString: String = s"$elementName $selector"
}

object HtmlElement {
//  Element name strings
  val formN = "form"
  val inputN = "input"
  val anchorN = "anchor"
  val divN = "div"
}