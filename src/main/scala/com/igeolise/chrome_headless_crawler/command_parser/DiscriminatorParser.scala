package com.igeolise.chrome_headless_crawler.command_parser

object DiscriminatorParser {
  private val regex = "(\\w+)(?: (.+))".r
  def unapply(command: String): Option[Discriminator] = {

    command match {
      case regex(name, value) =>
        name match {
          case "id" => Some(Id(value))
          case "name" => Some(Name(value))
          case "title" => Some(Title(value))
          case "value" => Some(Value(value))
          case "text" => Some(Text(value))
          case _ => None
        }
      case _ => None
    }
  }
}