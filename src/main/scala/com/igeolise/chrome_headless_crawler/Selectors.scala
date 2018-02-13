package com.igeolise.chrome_headless_crawler

import crawler.command_parser._

object Selectors {
  def toSelectorString(discriminator: Discriminator): String = {
    discriminator match {
      case Id(value) => s"[id=$value]"
      case Name(value) => s"[name=$value]"
      case Title(value) => s"[title=$value]"
      case Value(value) => s"[value=$value]"
      case ContainsText(value) => s":contains($value)"
    }
  }

  def toSelectorString(element: HtmlElement): String = {
    (element match {
      case CustomSelector(s) => s
      case Form(_) => "form"
      case Input(_) => "input"
      case Anchor(_) => "a"
      case Div(_) => "div"
      case Span(_) => "span"
      case TableDataCell(_) => "td"
      case TableRow(_) => "tr"
      case Label(_) => "label"
      case Paragraph(_) => "p"
      case AnyElement(_) => "*"
    }) + element.discriminator.map(d => discriminatorXPath(d)).getOrElse("")
  }
}
