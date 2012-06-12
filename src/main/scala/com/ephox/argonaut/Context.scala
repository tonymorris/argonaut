package com.ephox
package argonaut

import Json._

trait Context {
  val toList: List[ContextElement]

  def +:(e: ContextElement): Context =
    Context.build(e :: toList)
}

object Context extends Contexts

trait Contexts {
  private[argonaut] def build(x: List[ContextElement]): Context =
    new Context {
      val toList = x
    }

  def empty: Context =
    new Context {
      val toList = Nil
    }
}

sealed trait ContextElement {
  def json: Json =
    this match {
      case ArrayContext(j) => j
      case ObjectContext(_, j) => j
    }

  def field: Option[JsonField] =
    this match {
      case ArrayContext(_) => None
      case ObjectContext(f, _) => Some(f)
    }
}
private case class ArrayContext(j: Json) extends ContextElement
private case class ObjectContext(f: JsonField, j: Json) extends ContextElement

object ContextElement extends ContextElements

trait ContextElements {
  def arrayC(j: Json): ContextElement =
    ArrayContext(j)

  def objectC(f: JsonField, j: Json): ContextElement =
    ObjectContext(f, j)
}