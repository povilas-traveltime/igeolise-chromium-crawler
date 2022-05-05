package com.igeolise

import play.api.libs.json.{JsString, JsValue}

import scala.util.Try

object Helpers {

  implicit class JsValueOps(val e: JsValue) extends AnyVal {
    def stringValue: String = e.as[JsString].value
    def stringOpt = e.asOpt[JsString].map(_.value)
  }

  implicit class TryOps[A](val e: Try[A]) extends AnyVal {
    def leftMap[B](op: Throwable => B): Either[B, A] = e.toEither.left.map(op)
  }

  implicit class IdOps[A](val e: A) extends AnyVal {
    def |>[B](op: A => B) = op(e)
  }

}
