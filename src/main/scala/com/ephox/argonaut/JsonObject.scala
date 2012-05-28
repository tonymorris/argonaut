package com.ephox
package argonaut

import Json._
import scalaz._, Scalaz._

sealed trait JsonObject {
  val toMap: InsertionMap[JsonField, Json]

  def +(f: JsonField, j: Json): JsonObject =
    JsonObject(toMap ^+^ (f, j))

  def -(f: JsonField): JsonObject =
    JsonObject(toMap ^-^ f)

  def ++(o: JsonObject): JsonObject =
    JsonObject(toMap ++ o.toMap)

  def apply(f: JsonField): Option[Json] =
    toMap get f

  def withJsons(k: Json => Json): JsonObject =
    JsonObject(toMap map k)

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

  def fields: List[JsonField] =
    toMap.keys

  def size: Int =
    toMap.size
}

object JsonObject extends JsonObjects {
  private[argonaut] def apply(x: InsertionMap[JsonField, Json]): JsonObject =
    new JsonObject {
      val toMap = x
    }
}

trait JsonObjects {
  def empty: JsonObject =
    JsonObject(InsertionMap.empty)

  def single(f: JsonField, j: Json): JsonObject =
    empty + (f, j)

  def jsonObjectL(f: JsonField): JsonObject @> PossibleJson =
    InsertionMap.insertionMapL(f).xmapA(JsonObject(_), _.toMap)

  def jsonObjectPL(f: JsonField): JsonObject @?> Json =
    PLensT.somePLens compose ~jsonObjectL(f)

  implicit val JsonObjectInstances: Monoid[JsonObject] with Equal[JsonObject] =
    new Monoid[JsonObject] with Equal[JsonObject] {
      def zero = empty
      def append(j1: JsonObject, j2: => JsonObject) = j1 ++ j2
      def equal(j1: JsonObject, j2: JsonObject) =
        j1.toMap == j2.toMap
    }

}
