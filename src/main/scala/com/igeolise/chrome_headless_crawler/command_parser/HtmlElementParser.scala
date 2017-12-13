package crawler.command_parser

object HtmlElementParser {
  private val regex = "(\\w+)(?> having (.+))?".r
  private val twoStringRegex = "(\\w+) (.+)".r
  private val discriminatorParser = new MapOptionUnapply[String, Discriminator](DiscriminatorParser.unapply)
  def unapply(command: String): Option[HtmlElement] = {

    command match {
      case regex(name, NullToOption(discriminatorParser(discriminator))) =>
        name match {
          case "form" => Some(Form(discriminator))
          case "input" => Some(Input(discriminator))
          case "anchor" => Some(Anchor(discriminator))
          case "div" => Some(Div(discriminator))
          case "span" => Some(Span(discriminator))
          case "td" => Some(TableDataCell(discriminator))
          case "tr" => Some(TableRow(discriminator))
          case "label" => Some(Label(discriminator))
          case "anyElement" => Some(AnyElement(discriminator))
          case "paragraph" => Some(Paragraph(discriminator))
          case _ => None
        }
      case twoStringRegex("customSelector", value) => Some(CustomSelector(value))
      case _ => None
    }
  }
}