package crawler.command_parser

sealed trait HtmlElement {
  val discriminator: Option[Discriminator]
}
case class Form(discriminator: Option[Discriminator]) extends HtmlElement
case class Input(discriminator: Option[Discriminator]) extends HtmlElement
case class Anchor(discriminator: Option[Discriminator]) extends HtmlElement
case class Div(discriminator: Option[Discriminator]) extends HtmlElement
case class Span(discriminator: Option[Discriminator]) extends HtmlElement
case class TableDataCell(discriminator: Option[Discriminator]) extends HtmlElement
case class TableRow(discriminator: Option[Discriminator]) extends HtmlElement
case class Label(discriminator: Option[Discriminator]) extends HtmlElement
case class AnyElement(discriminator: Option[Discriminator]) extends HtmlElement
case class Paragraph(discriminator: Option[Discriminator]) extends HtmlElement
case class CustomSelector(selector: String) extends HtmlElement {
  val discriminator = None
}