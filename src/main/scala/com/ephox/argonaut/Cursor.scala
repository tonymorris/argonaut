package com.ephox
package argonaut

import scalaz._
import Json._
import Lens._
import CostateT._

sealed trait Cursor {
  private def parent: Parent =
    this match {
      case CJson(p, _, _) => p
      case CArray(p, _, _, _, _) => p
      case CObject(p, _, _, _) => p
    }

  /** Returns the array/object context of the current focus. */
  def context: Context =
    this match {
      case CJson(_, c, _) => c
      case CArray(_, c, _, _, _) => c
      case CObject(_, c, _, _) => c
    }

  /** Set the focus to the given value. */
  def focus: Json =
    this match {
      case CJson(_, _, j) => j
      case CArray(_, _, _, j, _) => j
      case CObject(_, _, _, (_, j)) => j
    }

  /** Update the focus with the given function. */
  def withFocus(k: Json => Json): Cursor =
    this match {
      case CJson(p, c, j) => {
        val jj = k(j)
        CJson(p, jj @: c, jj)
      }
      case CArray(p, c, l, j, r) => {
        val jj = k(j)
        CArray(p, jj @: c, l, k(j), r)
      }
      case CObject(p, c, o, (f, j)) => {
        val jj = k(j)
        CObject(p, jj @: c, o + (f, jj), (f, jj))
      }
    }

  /** Set the focus to the given value. */
  def :=(j: Json): Cursor =
    withFocus(_ => j)

  /** Move the cursor left in a JSON array. */
  def left: Option[Cursor] =
    this match {
      case CArray(p, c, l, j, r) => l match {
        case Nil => None
        case h::t => Some(CArray(p, h @: c, t, h, j::r))
      }
      case _ => None
    }

  /** Move the cursor right in a JSON array. */
  def right: Option[Cursor] =
    this match {
      case CArray(p, c, l, j, r) => r match {
        case Nil => None
        case h::t => Some(CArray(p, h @: c, j::t, h, r))
      }
      case _ => None
    }

  def first: Option[Cursor] =
    this match {
      case CArray(p, c, l, j, r) => {
        val h::t = l.reverse ::: j :: r
        Some(CArray(p, h @: c, Nil, h, t))
      }
      case _ => None
    }

  def last: Option[Cursor] =
    this match {
      case CArray(p, c, l, x, r) => {
        val h::t = r.reverse ::: x :: l
        Some(CArray(p, h @: c, t, h, Nil))
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
      case CObject(p, c, o, (f, j)) =>
        o(q) map (jj => CObject(p, (q, jj) @@: c, o, (q, jj)))
      case _ => None
    }

  /** Move the cursor down to a JSON object at the given field. */
  def --\(q: JsonField): Option[Cursor] =
    this match {
      case CJson(_, _, _) => None
      case CArray(p, c, l1, j, r1) => error("") // todo Some(PArray(p, l1, j, r2))
      case CObject(p, c, o, (f, j)) => error("") // todo Some(PCobject(p, o, (f, j)))
    }

  /** Move the cursor down to a JSON array at the first element satisfying the given predicate. */
  def -\(p: Json => Boolean): Option[Cursor] =
    error("")

  /** Move the cursor down to a JSON array at the given index. */
  def =\(n: Int): Option[Cursor] =
    error("")

  /** Move the cursor down to a JSON array at the first element. */
  def downArray: Option[Cursor] =
    =\(0)

  /** Move the cursor up one step to the parent context. */
  def up: Option[Cursor] = {
    lazy val (pp, cc, jj) =
      this match {
        case CJson(p, c, j) => (p, c, j)
        case CArray(p, c, l1, j, r1) => (p, c, jArray(l1.reverse ::: j :: r1))
        case CObject(p, c, o, _) => (p, c, jObject(o))
      }
    pp match {
      case PNone => None
      case PArray(q, l, _, r) =>
        Some(CArray(q, cc.unsafeup, l, jj, r))
      case PObject(q, oo, (f, _)) =>
        Some(CObject(q, cc.unsafeup, oo + (f, jj), (f, jj)))
    }
  }

  /** Unapplies the cursor to the top-level parent. */
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
private case class CJson(p: Parent, c: Context, j: Json) extends Cursor
private case class CArray(p: Parent, c: Context, ls: List[Json], x: Json, rs: List[Json]) extends Cursor
private case class CObject(p: Parent, c: Context, i: JsonObject, x: (JsonField, Json)) extends Cursor

private sealed trait Parent
private case object PNone extends Parent
private case class PArray(p: Parent, ls: List[Json], x: Json, rs: List[Json]) extends Parent
private case class PObject(p: Parent, i: JsonObject, x: (JsonField, Json)) extends Parent

object Cursor extends Cursors {
  def apply(j: Json): Cursor =
    CJson(PNone, Context.empty, j)
}

trait Cursors {

}

// todo

/*
sealed trait Cursor {

  import JsonIdentity._


  /** Move the cursor to the given sibling key in a JSON object */
  def --(q: JsonField): Option[Cursor] =
    this match {
      case CNull(_) => None
      case CBool(_, _) => None
      case CNumber(_, _) => None
      case CString(_, _) => None
      case CArray(_, _, _, _) => None
      case CObject(p, i, _) => i(q) map (j => CObject(p, i, (q, j)))
    }

  /** Move the cursor down to a JSON object at the given field. */
  def --\(q: JsonField): Option[Cursor] =
    this match {
      case CNull(_) => None
      case CBool(_, _) => None
      case CNumber(_, _) => None
      case CString(_, _) => None
      case CArray(p, l, x, r) => x.obj flatMap (o => o(q) map (w => CObject(CArrayParent(p, l, x, r), o, (q, w))))
      case CObject(p, i, v) => v._2.obj flatMap (o => o(q) map (w => CObject(CObjectParent(p, i, v), o, (q, w))))
    }

  /** Move the cursor down to a JSON array at the given index. */
  def -\(n: Int): Option[Cursor] = {
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
    -\(0)

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
  def cursor(j: Json): Option[Cursor] =
    j match {
      case JNull      => Some(CNull(NoParent))
      case JBool(b)   => Some(CBool(NoParent, b))
      case JNumber(n) => Some(CNumber(NoParent, n))
      case JString(s) => Some(CString(NoParent, s))
      case JArray(a)  => a match {
        case Nil => None
        case h::t => Some(CArray(NoParent, Nil, h, t))
      }
      case JObject(o) => o.toList match {
        case Nil => None
        case (f, jj)::_ => Some(CObject(NoParent, o, (f, jj)))
      }
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
}
       */
