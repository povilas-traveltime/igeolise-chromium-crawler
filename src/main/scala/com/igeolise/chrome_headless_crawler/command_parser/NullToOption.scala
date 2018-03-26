package com.igeolise.chrome_headless_crawler.command_parser

object NullToOption {
  def unapply[T](a: T): Option[Option[T]] = if (a == null) Some(None) else Some(Some(a))
}
