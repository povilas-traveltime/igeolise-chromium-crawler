package com.igeolise.chrome_headless_crawler.command_parser

object HtmlElementParser {
  private val elemWithDiscriminator = "(\\w+)(?> having (.+))?".r
  private val customElem = "(\\w+) (.+)".r
  def unapply(command: String): Option[HtmlElement] = {

    command match {
      case elemWithDiscriminator(name, discriminator) =>
        val discriminatorOption = Option(discriminator).flatMap { DiscriminatorParser.unapply }
        name match {
          case "form" => Some(Form(discriminatorOption))
          case "input" => Some(Input(discriminatorOption))
          case "anchor" => Some(Anchor(discriminatorOption))
          case "div" => Some(Div(discriminatorOption))
          case "span" => Some(Span(discriminatorOption))
          case "td" => Some(TableDataCell(discriminatorOption))
          case "tr" => Some(TableRow(discriminatorOption))
          case "label" => Some(Label(discriminatorOption))
          case "anyElement" => Some(AnyElement(discriminatorOption))
          case "paragraph" => Some(Paragraph(discriminatorOption))
          case _ => None
        }
      case customElem("customSelector", value) => Some(CustomSelector(value))
      case _ => None
    }
  }
}