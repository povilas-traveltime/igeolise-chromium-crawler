package com.igeolise.chrome_headless_crawler

import com.igeolise.chrome_headless_crawler.command_parser._

object Selectors {

  implicit class DiscriminatorExt(val discriminator: Discriminator) extends AnyVal {
    def toSelectorString: String = {
      discriminator match {
        case Id(value) => s"[id=$value]"
        case Name(value) => s"[name=$value]"
        case Title(value) => s"[title=$value]"
        case Value(value) => s"[value=$value]"
        case Text(value) => s":contains($value)"
      }
    }
  }

  implicit class HtmlElementExt(val element: HtmlElement) extends AnyVal {
    def toSelectorString: String = {
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
      }) + element.discriminator.map(_.toSelectorString).getOrElse("")
    }
  }

  def attributesToElement(attributes: Map[String, String]): HtmlElement = {
    val textContentSelector = attributes.get("textContent").map(t => s"textContent='$t'").getOrElse("")
    val hrefSelector = attributes.get("href").map("href=" + _).getOrElse("")
    val idSelector = attributes.get("id").map("id" + _).getOrElse("")
    val combined = Seq(textContentSelector, hrefSelector, idSelector).mkString(",")
    CustomSelector(s"*[$combined]")
  }
}
