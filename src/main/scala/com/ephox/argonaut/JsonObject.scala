package com.ephox
package argonaut

import Json._
import scalaz._, Scalaz._

sealed trait JsonObject {
  val fields: List[JsonField]
  val toMap: Map[JsonField, Json]

  /*
  val toMap: Map[JsonField, Json]

  def +(f: JsonField, j: Json): JsonObject =
    JsonObject(toMap + ((f, j)))

  def -(f: JsonField): JsonObject =
    JsonObject(toMap - f)

  def ++(o: JsonObject): JsonObject =
    JsonObject(toMap ++ o.toMap)

  def apply(f: JsonField): Option[Json] =
    toMap get f

  def withJsons(k: Json => Json): JsonObject =
    JsonObject(toMap mapValues k)

  def isEmpty: Boolean =
    toMap.isEmpty

  def isNotEmpty: Boolean =
    !isEmpty

  def toList: List[(JsonField, Json)] =
    toMap.toList

  def values: List[Json] =
    toList map (_._2)

  def kleisli: Kleisli[Option, JsonField, Json] =
    Kleisli(toMap get _)

  def fields: Set[JsonField] =
    toMap.keySet

  def size: Int =
    toMap.size
    */
}

object JsonObject extends JsonObjects {
  /*
  private[argonaut] def apply(x: Map[JsonField, Json]): JsonObject =
    new JsonObject {
      val toMap = x
    }
    */
}

trait JsonObjects {
  /*
  def empty: JsonObject = new JsonObject {
    val toMap = Map.empty[JsonField, Json]
  }

  def jsonObjectL(f: JsonField): JsonObject @> PossibleJson =
    Lens(o => Costate(_ match {
      case None => JsonObject(o.toMap - f)
      case Some(j) => JsonObject(o.toMap.updated(f, j))
    }, o(f)))

  def jsonObjectPL(f: JsonField): JsonObject @?> Json =
    PLensT.somePLens compose ~jsonObjectL(f)

  implicit val JsonObjectInstances: Monoid[JsonObject] with Equal[JsonObject] =
    new Monoid[JsonObject] with Equal[JsonObject] {
      def zero = empty
      def append(j1: JsonObject, j2: => JsonObject) = j1 ++ j2
      def equal(j1: JsonObject, j2: JsonObject) =
        j1.fields union j2.fields forall (f => j1(f) === j2(f))
    }
       */
}
