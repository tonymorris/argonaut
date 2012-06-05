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
      case CJson(j) => j
      case CArray(_, _, j, _) => j
      case CObject(_, _, (_, j)) => j
    }

  /** Update the focus with the given function. */
  def withFocus(k: Json => Json): Cursor =
    this match {
      case CJson(j) => {
        val jj = k(j)
        CJson(jj)
      }
      case CArray(p, l, j, r) => {
        val jj = k(j)
        CArray(p, l, k(jj), r)
      }
      case CObject(p, x, (f, j)) => {
        val jj = k(j)
        CObject(p, x, (f, jj))
      }
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
      case CArray(gp, l, j, r) => l match {
        case Nil => None
        case h::t => Some(CArray(gp, t, h, j::r))
      }
      case _ => None
    }

  /** Move the cursor right in a JSON array. */
  def right: Option[Cursor] =
    this match {
      case CArray(gp, l, j, r) => r match {
        case Nil => None
        case h::t => Some(CArray(gp, j::l, h, t))
      }
      case _ => None
    }
  
  /** Move the cursor to the first in a JSON array. */
  def first: Option[Cursor] =
    this match {
      case CArray(gp, l, j, r) => {
        val h::t = l.reverse ::: j :: r
        Some(CArray(gp, Nil, h, t))
      }
      case _ => None
    }

  /** Move the cursor to the last in a JSON array. */
  def last: Option[Cursor] =
    this match {
      case CArray(p, l, x, r) => {
        val h::t = r.reverse ::: x :: l
        Some(CArray(p, t, h, Nil))
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
      case CObject(gp, o, (f, j)) =>
        o(q) map (jj => CObject(gp, o + (f, j), (q, jj)))
      case _ => None
    }

  /** Move the cursor down to a JSON object at the given field. */
  def --\(q: JsonField): Option[Cursor] =
    focus.obj flatMap (o => o(q) map (jj => CObject(this, o, (q, jj))))

  /** Move the cursor down to a JSON array at the first element. */
  def downArray: Option[Cursor] =
    focus.array flatMap (_ match {
      case Nil => None
      case h::t => Some(CArray(this, Nil, h, t))
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
    go(true).right.toOption

  /** Move the cursor up one step to the parent context. */
  def up: Option[Cursor] =
    go(false).right.toOption

  /** Unapplies the cursor to the top-level parent. */
  def unary_- : Json = {
    @annotation.tailrec
    def u(c: Cursor): Json =
      c go false match {
        case Left(j) => j
        case Right(cc) => u(cc)
      }
    u(this)
  }

  private def go[X](dropfocus: Boolean): Either[Json, Cursor] = {
    // todo set updated flag in constructor
    def updatedtrueset(c: Cursor, j: Json): Cursor =
      c withFocus (_ => j)

    this match {
      case CJson(j) =>
        Left(j)
      case CArray(p, l, j, r) =>
        Right(updatedtrueset(p, jArray(l.reverse ::: (if(dropfocus) r else j :: r))))
      case CObject(p, o, (f, j)) =>
        Right(updatedtrueset(p, jObject(if(dropfocus) o - f else o + (f, j))))
    }
  }
}
private case class CJson(j: Json) extends Cursor
private case class CArray(gp: Cursor, ls: List[Json], x: Json, rs: List[Json]) extends Cursor
private case class CObject(p: Cursor, o: JsonObject, x: (JsonField, Json)) extends Cursor

object Cursor extends Cursors {
  def apply(j: Json): Cursor =
    CJson(j)
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

