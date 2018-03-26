package com.igeolise.chrome_headless_crawler.command_parser

import org.specs2.mutable.Specification

class DiscriminatorParserSpec extends Specification {
  "discriminatorParser" >> {
    "must fail on empty string" >> {
      def parsing = "" match {
        case DiscriminatorParser(d) => d
      }
      parsing must throwA[MatchError]
    }

    "must fail on unacceptable input" >> {
      def parsing = "shouldNotMatch" match {
        case DiscriminatorParser(d) => d
      }
      parsing must throwA[MatchError]
    }

    def mustParse[A <: Discriminator](discriminator: A, name: String, value: String) = {
      s"must parse '$name' correctly" >> {
        def parsing = s"$name $value" match {
          case DiscriminatorParser(d) => d
        }
        parsing mustEqual discriminator
      }
    }

    val value = "someValidValue"

    mustParse(Id(value), "id", value)
    mustParse(Name(value), "name", value)
    mustParse(Title(value), "title", value)
    mustParse(ContainsText(value), "containsText", value)
    mustParse(Value(value), "value", value)
  }
}