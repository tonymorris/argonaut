package com.ephox
package argonaut

import scalaz._, Scalaz._

trait FromJsonResult[A] {
  import FromJsonResult._

  val jvalue: Either[JError, A]

  def toOption: Option[A] =
    jvalue.right.toOption

  def toError: Option[JError] =
    jvalue.left.toOption

  def |(v: => A): A =
    jvalue.right getOrElse v

  def map[B](f: A => B): FromJsonResult[B] =
    FromJsonResult(jvalue.right map f)

  def flatMap[B](f: A => FromJsonResult[B]): FromJsonResult[B] =
    FromJsonResult(jvalue.right flatMap (f(_).jvalue))

  def mapError(f: JError => JError): FromJsonResult[A] =
    flatMapError(s => jsonError(f(s)))

  def flatMapError(f: JError => FromJsonResult[A]): FromJsonResult[A] =
    FromJsonResult(jvalue.left flatMap (f(_).jvalue))
}

object FromJsonResult extends FromJsonResults

trait FromJsonResults {
  type JError =
    String

  def apply[A](x: Either[JError, A]): FromJsonResult[A] = new FromJsonResult[A] {
    val jvalue = x
  }

  def jsonValue[A]: A => FromJsonResult[A] = a =>
    apply(Right(a))
  
  def jsonError[A]: JError => FromJsonResult[A] = message =>
    apply(Left(message))

  def jErrorL[A]: FromJsonResult[A] @?> JError =
    PLens(_.toError map (e => Costate(jsonError, e)))

  def jValueL[A]: FromJsonResult[A] @?> A =
    PLens(_.toOption map (a => Costate(jsonValue, a)))

  implicit def JsonValueMonad: Monad[FromJsonResult] = new Monad[FromJsonResult] {
    def point[A](a: => A) = jsonValue(a)
    def bind[A, B](a: FromJsonResult[A])(f: A => FromJsonResult[B]) = a flatMap f
    override def map[A, B](a: FromJsonResult[A])(f: A => B) = a map f
  }
}
