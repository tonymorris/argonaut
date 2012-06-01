package com.ephox
package argonaut

import Json._

trait Context {
  val toList: List[ContextElement]

  def +:(e: ContextElement): Context =
    Context.build(e :: toList)

  def up: Option[Context] =
    toList match {
      case Nil => None
      case _::t => Some(Context.build(t))
    }

  def @:(j: Json): Context =
    toList match {
      case Nil => this
      case ArrayContext(_)::t => Context.build(ArrayContext(j)::t)
      case ObjectContext(f, _)::t => Context.build(ObjectContext(f, j)::t)
    }

  def @@:(q: (JsonField, Json)): Context =
    toList match {
      case ObjectContext(_, _)::t => Context.build(ObjectContext(q._1, q._2)::t)
      case _ => this
    }

  // CAUTION
  // This function fails with empty context.
  // This function should only be used by internal code that is aware of invariants
  //   that are not exposed by public-facing API.
  // For example, a zipper that holds context, even though it is computable from the zipper context itself.
  private[argonaut] def unsafeup: Context =
    up getOrElse (sys.error("Invariant failed: unsafeup called on empty context"))
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