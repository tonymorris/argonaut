package com.ephox
package argonaut

/**
 * A data type representing possible <a href="http://www.json.org/">JSON</a> values.
 *
 * @author Tony Morris
 * @author Dylan Just
 * @author Mark Hibberd
 */
sealed trait Json {
  import Json._
  import JsonIdentity._

  /**
   * The catamorphism for the JSON value data type.
   */
  def fold[X](
    jsonNull: => X,
    jsonBool: Boolean => X,
    jsonNumber: JsonNumber => X,
    jsonString: String => X,
    jsonArray: JsonArray => X,
    jsonObject: JsonObject => X
  ): X =
    this match {
      case JNull      => jsonNull
      case JBool(b)   => jsonBool(b)
      case JNumber(n) => jsonNumber(n)
      case JString(s) => jsonString(s)
      case JArray(a)  => jsonArray(a)
      case JObject(o) => jsonObject(o)
    }

  /**
   * The name of the type of the JSON value.
   */
  def name: String =
    this match {
      case JNull      => "Null"
      case JBool(_)   => "Boolean"
      case JNumber(_) => "Number"
      case JString(_) => "String"
      case JArray(_)  => "Array"
      case JObject(_) => "Object"
    }

  /**
   * Attempts to encode this JSON value to another data type.
   */
  def encode[A](implicit e: EncodeJson[A]): EncodeResult[A] =
    e(this)

  /**
   * Compute a `String` representation for this JSON value.
   */
  override def toString =
    "Json{" +
        fold(
          "null",
          "bool[" + _ + "]",
          "number[" + _ + "]",
          "string[" + _ + "]",
          "array[" + _ + "]",
          "object[" + _ + "]"
        ) + "}"

}
import Json._

private case object JNull extends Json
private case class JBool(b: Boolean) extends Json
private case class JNumber(n: JsonNumber) extends Json
private case class JString(s: String) extends Json
private case class JArray(a: JsonArray) extends Json
private case class JObject(o: JsonObject) extends Json

object Json extends Jsons

/**
 * Constructors and other utilities for JSON values.
 *
 * @author Tony Morris
 * @author Dylan Just
 * @author Mark Hibberd
 */
trait Jsons {
  type JsonNumber = Double
  type JsonArray = List[Json]
  type JsonString = String
  type JsonField = String
  type JsonAssoc = (JsonField, Json)
  type JsonObjectMap = scalaz.InsertionMap[JsonField, Json]

  import scalaz._, Scalaz._, PLens._, CostateT._

  implicit def JsonJsonLike: JsonLike[Json] =
    new JsonLike[Json] {
      def isNull: Json => Boolean =
        _.fold(true, _ => false, _ => false, _ => false, _ => false, _ => false)

      def jBoolL: Json @?> Boolean =
        PLens(_.fold(None, z => Some(Costate(JBool, z)), _ => None, _ => None, _ => None, _ => None))

      def jNumberL: Json @?> JsonNumber =
        PLens(_.fold(None, _ => None, z => Some(Costate(JNumber, z)), _ => None, _ => None, _ => None))

      def jStringL: Json @?> JsonString =
        PLens(_.fold(None, _ => None, _ => None, z => Some(Costate(JString, z)), _ => None, _ => None))

      def jArrayL: Json @?> JsonArray =
        PLens(_.fold(None, _ => None, _ => None, _ => None, z => Some(Costate(JArray, z)), _ => None))

      def jObjectL: Json @?> JsonObject =
        PLens(_.fold(None, _ => None, _ => None, _ => None, _ => None, z => Some(Costate(JObject, z))))

      def jNull =
        JNull

      def jBool =
        JBool

      def jNumber =
        JNumber

      def jString =
        JString

      def jArray =
        JArray

      def jObject =
        JObject
    }

  import JsonIdentity._

  implicit def JsonInstances: Equal[Json] with Show[Json] =
    new Equal[Json] with Show[Json] {
      def equal(a1: Json, a2: Json) =
        a1 match {
              case JNull      => a2.isNull
              case JBool(b)   => a2.bool exists (_ == b)
              case JNumber(n) => a2.number exists (_ == n)
              case JString(s) => a2.string exists (_ == s)
              case JArray(a)  => a2.array exists (_ === a)
              case JObject(o) => a2.obj exists (_ === o)
            }

      def show(a: Json) = Show.showFromToString show a
    }
}
