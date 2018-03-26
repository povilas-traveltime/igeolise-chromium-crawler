package com.igeolise.chrome_headless_crawler.command_parser

sealed trait Discriminator {
  val value: String
}
case class Id(value: String) extends Discriminator
case class Name(value: String) extends Discriminator
case class Title(value: String) extends Discriminator
case class ContainsText(value: String) extends Discriminator
case class Value(value: String) extends Discriminator
