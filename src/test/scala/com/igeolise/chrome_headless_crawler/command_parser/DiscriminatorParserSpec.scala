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

    import Discriminator._

    mustParse(Id(value), idN, value)
    mustParse(Name(value), nameN, value)
    mustParse(Title(value), titleN, value)
    mustParse(Text(value), textN, value)
    mustParse(Value(value), valueN, value)
  }
}