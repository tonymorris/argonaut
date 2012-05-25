package com.ephox
package argonaut

import scalaz._, Scalaz._, LensT._

sealed trait EncodeResult[+A] {
  def json: Option[Json] =
    this match {
      case EncodeError(j, _) => Some(j)
      case EncodeValue(_) => None
    }

  def toEither: Either[String, A] =
    this match {
      case EncodeError(_, e) => Left(e)
      case EncodeValue(a) => Right(a)
    }

  def toValidation: Validation[String, A] =
    this match {
      case EncodeError(_, e) => Failure(e)
      case EncodeValue(a) => Success(a)
    }

  def value: Option[A] =
    toEither.right.toOption

  def error: Option[String] =
    toEither.left.toOption

  // Runs with a pretty English error message using the mismatched JSON value.
  def run: Either[String, A] =
    this match {
      case EncodeError(j, e) => Left("Expected: \"" + e + "\" Actual \"" + j.name + "\"")
      case EncodeValue(a) => Right(a)
    }

  def map[B](f: A => B): EncodeResult[B] =
    this match {
      case EncodeError(j, e) => EncodeError(j, e)
      case EncodeValue(a) => EncodeValue(f(a))
    }

  def flatMap[B](f: A => EncodeResult[B]): EncodeResult[B] =
    this match {
      case EncodeError(j, e) => EncodeError(j, e)
      case EncodeValue(a) => f(a)
    }

}
private case class EncodeError[+A](j: Json, s: String) extends EncodeResult[A]
private case class EncodeValue[+A](a: A) extends EncodeResult[A]

object EncodeResult extends EncodeResults {
  def apply[A](a: A): EncodeResult[A] =
    EncodeValue(a)
}

trait EncodeResults {
  def encodeError[A](j: Json, s: String): EncodeResult[A] =
    EncodeError(j, s)

  def encodeErrorL[A]: EncodeResult[A] @?> (Json, String) =
    PLens {
      case EncodeError(j, e) => Some(Costate(q => EncodeError(q._1, q._2), (j, e)))
      case EncodeValue(_) => None
    }

  def encodeErrorJL[A]: EncodeResult[A] @?> Json =
    encodeErrorL >=> ~firstLens

  def encodeErrorSL[A]: EncodeResult[A] @?> String =
    encodeErrorL >=> ~secondLens

  def encodeValueL[A]: EncodeResult[A] @?> A =
    PLens(_.value map (a => Costate(EncodeValue(_), a)))

  implicit def EncodeResultMonad: Monad[EncodeResult] = new Monad[EncodeResult] {
    def point[A](a: => A) = EncodeValue(a)
    def bind[A, B](a: EncodeResult[A])(f: A => EncodeResult[B]) = a flatMap f
    override def map[A, B](a: EncodeResult[A])(f: A => B) = a map f
  }
}

trait EncodeJson[+A] {
  import EncodeJson._

  def apply(j: Json): EncodeResult[A]

  def map[B](f: A => B): EncodeJson[B] =
    EncodeJson(apply(_) map f)

  def flatMap[B](f: A => EncodeJson[B]): EncodeJson[B] =
    EncodeJson(j => apply(j) flatMap (f(_)(j)))

  def kleisli: Kleisli[EncodeResult, Json, A] =
    Kleisli(apply(_))

  def &&&[B](x: EncodeJson[B]): EncodeJson[(A, B)] =
    EncodeJson(j => for {
      a <- this(j)
      b <- x(j)
    } yield (a, b))

  def split[B](x: EncodeJson[B]): Either[Json, Json] => EncodeResult[Either[A, B]] = {
    case Left(j) => this(j) map (Left(_))
    case Right(j) => x(j) map (Right(_))
  }

  def product[B](x: EncodeJson[B]): (Json, Json) => EncodeResult[(A, B)] = {
    case (j1, j2) => for {
      a <- this(j1)
      b <- x(j2)
    } yield (a, b)
  }
}

object EncodeJson extends EncodeJsons {
  def apply[A](f: Json => EncodeResult[A]): EncodeJson[A] =
    new EncodeJson[A] {
      def apply(j: Json) = f(j)
    }
}

trait EncodeJsons {
  import JsonIdentity._
  import EncodeResult._

  def encodej[A](f: Json => Option[A], n: String): EncodeJson[A] =
    EncodeJson(j =>
      f(j) match {
        case None => encodeError(j, n)
        case Some(a) => EncodeResult(a)
      }
    )

  def encodeArr[A](f: Json => A): EncodeJson[A] =
    EncodeJson(j => EncodeResult(f(j)))

  implicit def IdEncodeJson: EncodeJson[Json] =
    encodeArr(q => q)

  implicit def ListEncodeJson[A](implicit e: EncodeJson[A]): EncodeJson[List[A]] =
    EncodeJson(j =>
      j.array match {
        case None => encodeError(j, "[A]List[A]")
        case Some(js) => js.traverse[EncodeResult, A](e(_))
      }
    )

  implicit def StreamEncodeJson[A](implicit e: EncodeJson[A]): EncodeJson[Stream[A]] =
    EncodeJson(j =>
      j.array match {
        case None => encodeError(j, "[A]Stream[A]")
        case Some(js) => js.toStream.traverse[EncodeResult, A](e(_))
      }
    )

  implicit def StringEncodeJson: EncodeJson[String] =
    encodej(_.string, "String")

  implicit def DoubleEncodeJson: EncodeJson[Double] =
    encodej(x => if(x.isNull) Some(Double.NaN) else x.number, "Double")

  implicit def FloatEncodeJson: EncodeJson[Float] =
    encodej(x => if(x.isNull) Some(Float.NaN) else x.number map (_.toFloat), "Float")

  implicit def IntEncodeJson: EncodeJson[Int] =
    encodej(_.number map (_.toInt), "Int")

  implicit def LongEncodeJson: EncodeJson[Long] =
    encodej(_.number map (_.toLong), "Long")

  implicit def BooleanEncodeJson: EncodeJson[Boolean] =
    encodej(_.bool, "Boolean")

  implicit def CharEncodeJson: EncodeJson[Char] =
    encodej(_.string flatMap (s => if(s == 1) Some(s(0)) else None), "Char")

  implicit def JDoubleEncodeJson: EncodeJson[java.lang.Double] =
    encodej(_.number map (q => q), "java.lang.Double")

  implicit def JFloatEncodeJson: EncodeJson[java.lang.Float] =
    encodej(_.number map (_.toFloat), "java.lang.Float")

  implicit def JIntegerEncodeJson: EncodeJson[java.lang.Integer] =
    encodej(_.number map (_.toInt), "java.lang.Integer")

  implicit def JLongEncodeJson: EncodeJson[java.lang.Long] =
    encodej(_.number map (_.toLong), "java.lang.Long")

  implicit def JBooleanEncodeJson: EncodeJson[java.lang.Boolean] =
    encodej(_.bool map (q => q), "java.lang.Boolean")

  implicit def JCharacterEncodeJson: EncodeJson[java.lang.Character] =
    encodej(_.string flatMap (s => if(s == 1) Some(s(0)) else None), "java.lang.Character")

  implicit def OptionEncodeJson[A](implicit e: EncodeJson[A]): EncodeJson[Option[A]] =
    EncodeJson(j =>
      if(j.isNull)
        encodeError(j, "[A]Option[A]")
      else
        e(j) map (Some(_))
    )

  implicit def EitherEncodeJson[A, B](implicit ea: EncodeJson[A], eb: EncodeJson[B]): EncodeJson[Either[A, B]] =
    EncodeJson(j =>
      j.obj match {
        case None => encodeError(j, "[A, B]Either[A, B]")
        case Some(o) => o match {
          case ("Left", v) :: Nil => ea(v) map (Left(_))
          case ("Right", v) :: Nil => eb(v) map (Right(_))
          case _ => encodeError(j, "[A, B]Either[A, B]")
        }
      })

  implicit def ValidationEncodeJson[E, A](implicit ea: EncodeJson[E], eb: EncodeJson[A]): EncodeJson[Validation[E, A]] =
    EncodeJson(j =>
      j.obj match {
        case None => encodeError(j, "[E, A]Validation[E, A]")
        case Some(o) => o match {
          case ("Failure", v) :: Nil => ea(v) map (Validation.failure(_))
          case ("Success", v) :: Nil => eb(v) map (Validation.success(_))
          case _ => encodeError(j, "[E, A]Validation[E, A]")
        }
      })

  implicit def MapEncodeJson[V](implicit e: EncodeJson[V]): EncodeJson[Map[String, V]] =
    EncodeJson(j =>
      j.obj match {
        case None => encodeError(j, "[V]Map[String, V]")
        case Some(js) => js.traverse[EncodeResult, (String, V)]{
          case (k, v) => e(v) map ((k, _))
        } map (_.toMap)
      }
    )

  implicit def SetEncodeJson[A](implicit e: EncodeJson[A]): EncodeJson[Set[A]] =
    EncodeJson(j =>
      j.array match {
        case None => encodeError(j, "[A]Set[A]")
        case Some(js) => js.toSet.traverse[EncodeResult, A](e(_))
      }
    )

  implicit def Tuple2EncodeJson[A, B](implicit ea: EncodeJson[A], eb: EncodeJson[B]): EncodeJson[(A, B)] =
    EncodeJson(j =>
      j.array match {
        case Some(a::b::Nil) => for {
          aa <- ea(a)
          bb <- eb(b)
        } yield (aa, bb)
        case _ => encodeError(j, "[A, B](A, B)")
      })

  implicit def Tuple3EncodeJson[A, B, C](implicit ea: EncodeJson[A], eb: EncodeJson[B], ec: EncodeJson[C]): EncodeJson[(A, B, C)] =
    EncodeJson(j =>
      j.array match {
        case Some(a::b::c::Nil) => for {
          aa <- ea(a)
          bb <- eb(b)
          cc <- ec(c)
        } yield (aa, bb, cc)
        case _ => encodeError(j, "[A, B, C](A, B, C)")
      })

  implicit def EncodeJsonMonad: Monad[EncodeJson] = new Monad[EncodeJson] {
    def point[A](a: => A) = EncodeJson(_ => EncodeValue(a))
    def bind[A, B](a: EncodeJson[A])(f: A => EncodeJson[B]) = a flatMap f
    override def map[A, B](a: EncodeJson[A])(f: A => B) = a map f
  }

}

trait DecodeJson[-A] {
  def name: String

  def apply(a: A): Json

  def contramap[B](f: B => A): DecodeJson[B] =
    DecodeJson(b => apply(f(b)), name)
}

object DecodeJson extends DecodeJsons {
  def apply[A](f: A => Json, n: String): DecodeJson[A] =
    new DecodeJson[A] {
      def name = n
      def apply(a: A) = f(a)
    }
}

trait DecodeJsons {
  import JsonLike._

  implicit def IdDecodeJson: DecodeJson[Json] =
    DecodeJson(q => q, "Json")

  implicit def ListDecodeJson[A](implicit e: DecodeJson[A]): DecodeJson[List[A]] =
    DecodeJson(a => jArray[Json](a map (e(_))), "[A]List[A]")

  implicit def StreamDecodeJson[A](implicit e: DecodeJson[A]): DecodeJson[Stream[A]] =
    DecodeJson(a => jArray[Json](a.toList map (e(_))), "[A]Stream[A]")

  implicit def StringDecodeJson: DecodeJson[String] =
    DecodeJson(jString[Json], "String")

  implicit def DoubleDecodeJson: DecodeJson[Double] =
    DecodeJson(jNumber[Json], "Double")

  implicit def FloatDecodeJson: DecodeJson[Float] =
    DecodeJson(a => jNumber[Json](a.toFloat), "Float")

  implicit def IntDecodeJson: DecodeJson[Int] =
    DecodeJson(a => jNumber[Json](a.toInt), "Int")

  implicit def LongDecodeJson: DecodeJson[Long] =
    DecodeJson(a => jNumber[Json](a.toLong), "Long")

  implicit def BooleanDecodeJson: DecodeJson[Boolean] =
    DecodeJson(jBool[Json], "Boolean")

  implicit def CharDecodeJson: DecodeJson[Char] =
    DecodeJson(a => jString[Json](a.toString), "Char")

  implicit def JDoubleDecodeJson: DecodeJson[java.lang.Double] =
    DecodeJson(a => jNumber[Json](a.doubleValue), "java.lang.Double")

  implicit def JFloatDecodeJson: DecodeJson[java.lang.Float] =
    DecodeJson(a => jNumber[Json](a.floatValue.toDouble), "java.lang.Float")

  implicit def JIntegerDecodeJson: DecodeJson[java.lang.Integer] =
    DecodeJson(a => jNumber[Json](a.intValue.toDouble), "java.lang.Integer")

  implicit def JLongDecodeJson: DecodeJson[java.lang.Long] =
    DecodeJson(a => jNumber[Json](a.longValue.toDouble), "java.lang.Long")

  implicit def JBooleanDecodeJson: DecodeJson[java.lang.Boolean] =
    DecodeJson(a => jBool[Json](a.booleanValue), "java.lang.Boolean")

  implicit def JCharacterDecodeJson: DecodeJson[Char] =
    DecodeJson(a => jString[Json](a.toString), "java.lang.Character")

  implicit def OptionDecodeJson[A](implicit e: DecodeJson[A]): DecodeJson[Option[A]] =
    DecodeJson(_ match {
      case None => jNull[Json]
      case Some(a) => e(a)
    }, "[A]Option[A]")

  implicit def EitherDecodeJson[A, B](implicit ea: DecodeJson[A], eb: DecodeJson[B]): DecodeJson[Either[A, B]] =
    DecodeJson(_ match {
      case Left(a) => jSingleObject[Json]("Left", ea(a))
      case Right(b) => jSingleObject[Json]("Right", eb(b))
    }, "[A, B]Either[A, B]")

  implicit def ValidationDecodeJson[E, A](implicit ea: DecodeJson[E], eb: DecodeJson[A]): DecodeJson[Validation[E, A]] =
    DecodeJson(_ fold (
      failure = e => jSingleObject[Json]("Failure", ea(e))
    , success = a => jSingleObject[Json]("Success", eb(a))
    ), "[E, A]Validation[E, A]")

  implicit def MapDecodeJson[V](implicit e: DecodeJson[V]): DecodeJson[Map[String, V]] =
    DecodeJson(ListDecodeJson[(String, V)] contramap ((_: Map[String, V]).toList) apply _, "[V]Map[String, V]")

  implicit def SetDecodeJson[A](implicit e: DecodeJson[A]): DecodeJson[Set[A]] =
    DecodeJson(ListDecodeJson[A] contramap ((_: Set[A]).toList) apply _, "[A]Set[A]")

  implicit def Tuple2DecodeJson[A, B](implicit ea: DecodeJson[A], eb: DecodeJson[B]): DecodeJson[(A, B)] =
    DecodeJson({
      case (a, b) => jArray[Json](List(ea(a), eb(b)))
    }, "[A, B](A, B)")

  implicit def Tuple3DecodeJson[A, B, C](implicit ea: DecodeJson[A], eb: DecodeJson[B], ec: DecodeJson[C]): DecodeJson[(A, B, C)] =
    DecodeJson({
      case (a, b, c) => jArray[Json](List(ea(a), eb(b), ec(c)))
    }, "[A, B, C](A, B, C)")


  implicit def DecodeJsonContra: Contravariant[DecodeJson] = new Contravariant[DecodeJson] {
    def contramap[A, B](r: DecodeJson[A])(f: B => A) = r contramap f
  }
}