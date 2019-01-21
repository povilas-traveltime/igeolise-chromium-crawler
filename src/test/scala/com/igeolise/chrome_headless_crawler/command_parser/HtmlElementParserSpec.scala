package com.igeolise.chrome_headless_crawler.command_parser

import org.specs2.mutable.Specification

class HtmlElementParserSpec extends Specification {
  "htmlElementParser" >> {
    "must fail on empty string" >> {
      def parsing = "" match {
        case HtmlElementParser(e) => e
      }

      parsing must throwA[MatchError]
    }

    "must fail on unacceptable input" >> {
      def parsing = "shouldNotMatch" match {
        case HtmlElementParser(e) => e
      }

      parsing must throwA[MatchError]
    }

    "must not fail parsing a discriminator" >> {
      val id = "someId"

      def parsing = s"form having id $id" match {
        case HtmlElementParser(e) => e
      }

      parsing mustEqual Form(Some(Id(id)))
    }

    def mustParse[A <: HtmlElement](element: A, name: String) = {
      s"must parse '$name' correctly" >> {
        def parsing = s"$name" match {
          case HtmlElementParser(e) => e
        }

        parsing mustEqual element
      }
    }

    mustParse(Form(None), "form")
    mustParse(Input(None), "input")
    mustParse(Anchor(None), "anchor")
    mustParse(Div(None), "div")
    mustParse(Span(None), "span")
    mustParse(TableDataCell(None), "td")
    mustParse(TableRow(None), "tr")
    mustParse(Label(None), "label")
    mustParse(AnyElement(None), "anyElement")
    mustParse(Paragraph(None), "paragraph")
    mustParse(CustomSelector("tt"), "customSelector tt")
    mustParse(CustomElement("someName", None), "customElement someName")
  }
}