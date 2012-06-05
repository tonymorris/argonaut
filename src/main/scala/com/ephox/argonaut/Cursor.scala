package com.ephox
package argonaut

import scalaz._
import Json._
import ContextElement._
import Lens._
import CostateT._

sealed trait Cursor {
  /** Set the focus to the given value. */
  def focus: Json =
    this match {
      case CJson(_, j) => j
      case CArray(_, _, _, j, _) => j
      case CObject(_, _, _, (_, j)) => j
    }

  /** Update the focus with the given function. */
  def withFocus(k: Json => Json): Cursor =
    this match {
      case CJson(_, j) =>
        CJson(true, k(j))
      case CArray(_, p, l, j, r) => 
        CArray(true, p, l, k(j), r)
      case CObject(_, p, x, (f, j)) => 
        CObject(true, p, x, (f, k(j)))
    }

  /** Set the focus to the given value (alias for `:=`). */
  def set(j: Json): Cursor =
    withFocus(_ => j)

  /** Set the focus to the given value (alias for `set`). */
  def :=(j: Json): Cursor =
    set(j)

  /** Move the cursor left in a JSON array. */
  def left: Option[Cursor] =
    this match {
      case CArray(_, p, l, j, r) => l match {
        case Nil => None
        case h::t => Some(CArray(false, p, t, h, j::r))
      }
      case _ => None
    }

  /** Move the cursor right in a JSON array. */
  def right: Option[Cursor] =
    this match {
      case CArray(_, p, l, j, r) => r match {
        case Nil => None
        case h::t => Some(CArray(false, p, j::l, h, t))
      }
      case _ => None
    }
  
  /** Move the cursor to the first in a JSON array. */
  def first: Option[Cursor] =
    this match {
      case CArray(_, p, l, j, r) => {
        val h::t = l.reverse ::: j :: r
        Some(CArray(false, p, Nil, h, t))
      }
      case _ => None
    }

  /** Move the cursor to the last in a JSON array. */
  def last: Option[Cursor] =
    this match {
      case CArray(_, p, l, x, r) => {
        val h::t = r.reverse ::: x :: l
        Some(CArray(false, p, t, h, Nil))
      }
      case _ => None
    }

  /** Move the cursor left in a JSON array the given number of times. A negative value will move the cursor right. */
  def -<-:(n: Int): Option[Cursor] =
    if(n < 0)
      :->-(-n)
    else {
      @annotation.tailrec
      def r(x: Int, c: Option[Cursor]): Option[Cursor] =
        if (x == 0)
          c
        else
          r(x - 1, c flatMap (_.left))
      r(n, Some(this))
    }

  /** Move the cursor right in a JSON array the given number of times. A negative value will move the cursor left. */
  def :->-(n: Int): Option[Cursor] =
    if(n > 0)
      -<-:(-n)
    else {
      @annotation.tailrec
      def r(x: Int, c: Option[Cursor]): Option[Cursor] =
        if (x == 0)
          c
        else
          r(x + 1, c flatMap (_.right))
      r(n, Some(this))
    }

  /** Move the cursor left in a JSON array until the given predicate matches the focus. */
  def ?<-:(p: Json => Boolean): Option[Cursor] = {
    @annotation.tailrec
    def r(c: Option[Cursor]): Option[Cursor] =
      c match {
        case None => None
        case Some(w) => r(if(p(w.focus)) Some(w) else w.left)
      }

    r(left)
  }

  /** Move the cursor right in a JSON array until the given predicate matches the focus. */
  def :->?(p: Json => Boolean): Option[Cursor] = {
    @annotation.tailrec
    def r(c: Option[Cursor]): Option[Cursor] =
      c match {
        case None => None
        case Some(w) => r(if(p(w.focus)) Some(w) else w.right)
      }

    r(right)
  }
  
  /** Move the cursor to the given sibling key in a JSON object */
  def --(q: JsonField): Option[Cursor] =
    this match {
      case CObject(u, p, o, (f, j)) =>
        o(q) map (jj => CObject(u, p, o + (f, j), (q, jj)))
      case _ => None
    }

  /** Move the cursor down to a JSON object at the given field. */
  def --\(q: JsonField): Option[Cursor] =
    focus.obj flatMap (o => o(q) map (jj => CObject(false, this, o, (q, jj))))

  /** Move the cursor down to a JSON array at the first element. */
  def downArray: Option[Cursor] =
    focus.array flatMap (_ match {
      case Nil => None
      case h::t => Some(CArray(false, this, Nil, h, t))
    })

  /** Move the cursor down to a JSON array at the first element satisfying the given predicate. */
  def -\(p: Json => Boolean): Option[Cursor] =
    downArray flatMap (_ :->? p)

  /** Move the cursor down to a JSON array at the given index. */
  def =\(n: Int): Option[Cursor] =
    downArray flatMap (_ :->- n)

  /** Deletes the JSON value at focus and moves up to parent (alias for `delete`). */
  def unary_! : Option[Cursor] =
    delete

  /** Deletes the JSON value at focus and moves up to parent (alias for `unary_!`). */
  def delete: Option[Cursor] =
    go(true, _ => false).right.toOption

  /** Move the cursor up one step to the parent context. */
  def up: Option[Cursor] =
    go(false, u => u).right.toOption

  /** Unapplies the cursor to the top-level parent. */
  def unary_- : Json = {
    @annotation.tailrec
    def u(c: Cursor): Json =
      c go (false, _ => false) match {
        case Left(j) => j
        case Right(cc) => u(cc)
      }
    u(this)
  }

  private def go[X](dropfocus: Boolean, k: Boolean => Boolean): Either[Json, Cursor] = {
    // todo set updated flag in constructor
    def updatedtrueset(c: Cursor, j: Json): Cursor =
      this match {
        case CJson(u, _) => 
          CJson(k(u), j)
        case CArray(u, p, l, _, r) => 
          CArray(k(u), p, l, j, r)
        case CObject(u, p, x, (f, _)) => 
          CObject(k(u), p, x, (f, j))
      }

    this match {
      case CJson(_, j) =>
        Left(j)
      case CArray(u, p, l, j, r) =>
        Right(updatedtrueset(p, jArray(l.reverse ::: (if(dropfocus) r else j :: r))))
      case CObject(u, p, o, (f, j)) =>
        Right(updatedtrueset(p, jObject(if(dropfocus) o - f else o + (f, j))))
    }
  }
}
private case class CJson(u: Boolean, j: Json) extends Cursor
private case class CArray(u: Boolean, p: Cursor, ls: List[Json], x: Json, rs: List[Json]) extends Cursor
private case class CObject(u: Boolean, p: Cursor, o: JsonObject, x: (JsonField, Json)) extends Cursor

object Cursor extends Cursors {
  def apply(j: Json): Cursor =
    CJson(false, j)
}

trait Cursors {

}

  // lenses
  // upL: Cursor @?> Cursor
  // leftsL: Cursor @?> List[Json]
  // rightsL: Cursor @?> List[Json]
  // focusL: Cursor @?> Json
  // fieldL(k: JsonField): Cursor @> Option[Json]
  // fieldPL(k: JsonField): Cursor @?> Json
  

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

