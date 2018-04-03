package com.igeolise.chrome_headless_crawler.command_parser

sealed abstract class Discriminator(val discriminatorName: String) {
  val value: String
  override def toString: String = s"$discriminatorName $value"
}
case class Id(value: String) extends Discriminator("id")
case class Name(value: String) extends Discriminator("name")
case class Title(value: String) extends Discriminator("title")
case class ContainsText(value: String) extends Discriminator("containsText")
case class Value(value: String) extends Discriminator("value")
