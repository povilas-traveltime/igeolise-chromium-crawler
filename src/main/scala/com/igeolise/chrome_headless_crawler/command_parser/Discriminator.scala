package com.igeolise.chrome_headless_crawler.command_parser

sealed abstract class Discriminator(val discriminatorName: String) {
  val value: String
  override def toString: String = s"$discriminatorName $value"
}

import Discriminator._

case class Id(value: String) extends Discriminator(idN)
case class Name(value: String) extends Discriminator(nameN)
case class Title(value: String) extends Discriminator(titleN)
case class Text(value: String) extends Discriminator(textN)
case class Value(value: String) extends Discriminator(valueN)

object Discriminator {
//  Discriminator name strings
  val idN = "id"
  val nameN = "name"
  val titleN = "title"
  val textN = "text"
  val valueN = "value"
}