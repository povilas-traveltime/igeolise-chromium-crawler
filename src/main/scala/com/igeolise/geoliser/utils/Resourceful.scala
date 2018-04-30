package com.igeolise.geoliser.utils

/**
  * Credit to Denys Shabalin:
  * https://gist.github.com/densh/75d2d3063571d89ee34e161b4a61c74a
  */
object Resourceful {
  type Resource = AutoCloseable

  @annotation.implicitNotFound(msg = "Resource acquisition requires a scope.")
  final class ResourceScope extends Resource {
    private[this] var resources: List[Resource] = Nil

    def acquire(res: Resource): Unit = resources ::= res

    def close(): Unit = resources match {
      case Nil => ()
      case h :: t =>
        resources = t
        try h.close()
        finally close()
    }
  }

  object ResourceScope {
    def apply[T](f: ResourceScope => T): T = {
      val scope = new ResourceScope
      try f(scope)
      finally scope.close()
    }
  }

  def acquire[R <: Resource](res: R)(implicit in: ResourceScope): R = {
    in.acquire(res)
    res
  }
}
