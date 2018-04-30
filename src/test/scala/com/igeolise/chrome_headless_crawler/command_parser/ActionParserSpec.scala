package com.igeolise.chrome_headless_crawler.command_parser

import org.specs2.mutable.Specification

class ActionParserSpec extends Specification {
  "actionParser" >> {
    "must fail on empty string" >> {
      def parsing = "" match {
        case ActionParser(d) => d
      }
      parsing must throwA[MatchError]
    }

    "must fail on unacceptable input" >> {
      def parsing = "shouldNotMatch" match {
        case ActionParser(d) => d
      }
      parsing must throwA[MatchError]
    }

    "must not fail parsing element" >> {
      def parsing = "in div" match {
        case ActionParser(a) => a
      }
      parsing mustEqual In(Div(None))
    }

    def mustParse[A <: Action](action: A, name: String, stringTail: Option[String]) = {
      s"must parse '$name' correctly" >> {
        val parseString = stringTail.fold(name) { s => s"$name $s" }
        println(parseString)
        def parsing = parseString match {
          case ActionParser(d) => d
        }
        parsing mustEqual action
      }
    }

    val element = Div(None)
    val elementStringTail = Some("div")
    val stringValue = "some string"
    val stringValueTail = Some(stringValue)
    val stringValueContinuous = "someString"
    val stringValueContinuousTail = Some(stringValueContinuous)

    import Action._

    mustParse(In(element), inN, elementStringTail)
    mustParse(TypeIn(stringValue), typeInN, stringValueTail)
    mustParse(Click, clickN, None)
    mustParse(ClickDownload, clickDownloadN, None)
    mustParse(NavigateTo(stringValueContinuous), navitateToN, stringValueContinuousTail)
    mustParse(NavigateToDownload("some:url", Some(Credentials("u", "p"))), navigateToDownloadN, Some("some:url u p"))
    mustParse(OnCurrentPage, onCurrentPageN, None)
    mustParse(Up, upN, None)
    mustParse(InAll(element), inAllN, elementStringTail)
    mustParse(FindContainingInLastResult(stringValue), findContainintInLastResultN, Some(stringValue))
  }
}
