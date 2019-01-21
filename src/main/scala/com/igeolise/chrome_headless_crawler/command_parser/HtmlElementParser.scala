package com.igeolise.chrome_headless_crawler.command_parser

object HtmlElementParser {
  private val elemWithDiscriminator = "(\\w+)(?> having (.+))?".r
  private val customSelector = "(\\w+) (.+)".r
  private val customElement = "(\\w+) ([a-zA-Z0-9_\\-]+)(?> having (.+))?".r
  def unapply(command: String): Option[HtmlElement] = {

    command match {
      case elemWithDiscriminator(name, discriminatorString) =>
        val maybeDiscriminator = Option(discriminatorString).flatMap { DiscriminatorParser.unapply }
        name match {
          case "form" => Some(Form(maybeDiscriminator))
          case "input" => Some(Input(maybeDiscriminator))
          case "anchor" => Some(Anchor(maybeDiscriminator))
          case "div" => Some(Div(maybeDiscriminator))
          case "span" => Some(Span(maybeDiscriminator))
          case "td" => Some(TableDataCell(maybeDiscriminator))
          case "tr" => Some(TableRow(maybeDiscriminator))
          case "label" => Some(Label(maybeDiscriminator))
          case "anyElement" => Some(AnyElement(maybeDiscriminator))
          case "paragraph" => Some(Paragraph(maybeDiscriminator))
          case _ => None
        }

      case customSelector("customSelector", value) => Some(CustomSelector(value))
      case customElement("customElement", name, discriminatorString) =>
        val maybeDiscriminator = Option(discriminatorString).flatMap { DiscriminatorParser.unapply }
        Some(CustomElement(name, maybeDiscriminator))
      case _ => None
    }
  }
}