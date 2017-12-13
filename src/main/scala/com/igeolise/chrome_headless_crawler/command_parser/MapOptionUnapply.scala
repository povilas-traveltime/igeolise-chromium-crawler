package crawler.command_parser

class MapOptionUnapply[A, B](f: A => Option[B]) {
  def unapply(v: Option[A]): Option[Option[B]] = {
    Some(v.flatMap(f))
  }
}
