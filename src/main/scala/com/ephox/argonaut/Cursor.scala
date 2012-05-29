package com.ephox
package argonaut

import scalaz._
import JsonLike._
import Json._
import Lens._
import CostateT._

// todo
sealed trait ContextElement
private case class ArrayContext(j: Json) extends ContextElement
private case class ObjectContext(f: JsonField, j: Json) extends ContextElement

sealed trait Cursor {
  private def parent: Parent =
    this match {
      case CNull(p) => p
      case CBool(p, _) => p
      case CNumber(p, _) => p
      case CString(p, _) => p
      case CArray(p, _, _, _) => p
      case CObject(p, _, _) => p
    }

  import JsonIdentity._

  def focus: Json =
    this match {
      case CNull(_) => jNull[Json]
      case CBool(_, a) => jBool[Json](a)
      case CNumber(_, n) => jNumber[Json](n)
      case CString(_, s) => jString[Json](s)
      case CArray(_, _, j, _) => j
      case CObject(_, _, (_, j)) => j
    }

  def context: List[ContextElement] = {
    @annotation.tailrec
    def c(p: Parent, a: List[ContextElement]): List[ContextElement] =
      p match {
        case NoParent => a
        case CArrayParent(q, _, j, _) => c(q, ArrayContext(j)::a)
        case CObjectParent(q, _, (f, j)) => c(q, ObjectContext(f, j)::a)
      }

    c(parent, Nil)
  }

  /** Move the cursor left in a JSON array. */
  def left: Option[Cursor] =
    this match {
      case CNull(_) => None
      case CBool(_, _) => None
      case CNumber(_, _) => None
      case CString(_, _) => None
      case CArray(p, l, x, r) => l match {
        case Nil => None
        case h::t => Some(CArray(p, t, h, x::r))
      }
      case CObject(_, _, _) => None
    }

  /** Move the cursor right in a JSON array. */
  def right: Option[Cursor] =
    this match {
      case CNull(_) => None
      case CBool(_, _) => None
      case CNumber(_, _) => None
      case CString(_, _) => None
      case CArray(p, l, x, r) => r match {
        case Nil => None
        case h::t => Some(CArray(p, x::l, h, t))
      }
      case CObject(_, _, _) => None
    }

  /** Move the cursor to the given key in a JSON object */
  def field(q: JsonField): Option[Cursor] =
    this match {
      case CNull(_) => None
      case CBool(_, _) => None
      case CNumber(_, _) => None
      case CString(_, _) => None
      case CArray(_, _, _, _) => None
      case CObject(p, i, _) => i(q) map (j => CObject(p, i, (q, j)))
    }

  /** Move the cursor down to a JSON object at the given field. */
  def ->>-(q: JsonField): Option[Cursor] =
    this match {
      case CNull(_) => None
      case CBool(_, _) => None
      case CNumber(_, _) => None
      case CString(_, _) => None
      case CArray(p, l, x, r) => x.obj flatMap (o => o(q) map (w => CObject(CArrayParent(p, l, x, r), o, (q, w))))
      case CObject(p, i, v) => v._2.obj flatMap (o => o(q) map (w => CObject(CObjectParent(p, i, v), o, (q, w))))
    }

  /** Move the cursor down to a JSON array at the given index. */
  def ->-(n: Int): Option[Cursor] = {
    @annotation.tailrec
    def right0(
                 z: Option[(List[Json], Json, List[Json])]
               , x: Int
             ): Option[(List[Json], Json, List[Json])] =
      if(x == 0)
        z
      else
        right0(z flatMap {
          case (l1, x1, r1) => r1 match {
            case Nil => None
            case h::t => Some((x1::l1, h, t))
          }
        }, x - 1)
    this match {
      case CNull(_) => None
      case CBool(_, _) => None
      case CNumber(_, _) => None
      case CString(_, _) => None
      case CArray(p, l, x, r) => x.array flatMap {
        case Nil => None
        case h::t => right0(Some(Nil, h, t), n) map {
          case (l1, x1, r1) => CArray(CArrayParent(p, l, x, r), l1, x1, r1)
        }
      }
      case CObject(p, i, v) => v._2.array flatMap {
        case Nil => None
        case h::t => right0(Some(Nil, h, t), n) map {
          case (l1, x1, r1) => CArray(CObjectParent(p, i, v), l1, x1, r1)
        }
      }
    }
  }

  /** Move the cursor down to a JSON array at the first element. */
  def downArray: Option[Cursor] =
    ->-(0)

  /** Move the cursor up one step to the parent context. */
  def up: Option[Cursor] = {
    def unf: Json =
      this match {
        case CNull(_) => jNull[Json]
        case CBool(_, a) => jBool[Json](a)
        case CNumber(_, n) => jNumber[Json](n)
        case CString(_, s) => jString[Json](s)
        case CArray(_, l, x, r) => jArray[Json](l.reverse ::: x :: r)
        case CObject(_, i, (f, j)) => jObject[Json](i + (f, j))
      }
    parent match {
      case NoParent => None
      case CArrayParent(q, l, _, r) =>
        Some(CArray(q, l, unf, r))
      case CObjectParent(q, i, (f, _)) =>
        Some(CObject(q, i, (f, unf)))
    }
  }

  def unary_- : Json = {
    @annotation.tailrec
    def u(c: Cursor, j: Json): Json =
      c.up match {
        case None => j
        case Some(e) => u(e, e.focus)
      }
    u(this, focus)
  }
}
private sealed trait Parent
private case object NoParent extends Parent
private case class CArrayParent(p: Parent, ls: List[Json], x: Json, rs: List[Json]) extends Parent
private case class CObjectParent(p: Parent, i: JsonObject, x: (JsonField, Json)) extends Parent

private case class CNull(p: Parent) extends Cursor
private case class CBool(p: Parent, b: Boolean) extends Cursor
private case class CNumber(p: Parent, n: JsonNumber) extends Cursor
private case class CString(p: Parent, s: String) extends Cursor
private case class CArray(p: Parent, ls: List[Json], x: Json, rs: List[Json]) extends Cursor
private case class CObject(p: Parent, i: JsonObject, x: (JsonField, Json)) extends Cursor

object Cursor extends Cursors

trait Cursors {
  /*
  def parentL: Cursor @?> Cursor =
    PLens(_.parent map (w => Costate(z => new Cursor {
      val parent = Some(z)
      val lefts = w.lefts
      val focus = w.focus
      val rights = w.rights
    }, w)))

  val leftsL: Cursor @> List[Json] =
    Lens(w => Costate(z => new Cursor {
      val parent = w.parent
      val lefts = z
      val focus = w.focus
      val rights = w.rights
    }, w.lefts))

  def focusL: Cursor @> Json =
    Lens(w => Costate(z => new Cursor {
      val parent = w.parent
      val lefts = w.lefts
      val focus = z
      val rights = w.rights
    }, w.focus))

  def rightsL: Cursor @> List[Json] =
    Lens(w => Costate(z => new Cursor {
      val parent = w.parent
      val lefts = w.lefts
      val focus = w.focus
      val rights = z
    }, w.rights))
            */
}
