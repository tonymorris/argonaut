package com.ephox
package argonaut

import scalaz._, Scalaz._, Free._
import Json._

sealed trait Shift {
  private[argonaut] def shift(c: Cursor): Trampoline[ShiftResult]

  def run(c: Cursor): ShiftResult =
    shift(c).run

  def <|(j: Option[Json]): ShiftResult =
    j match {
      case None => ShiftResult(ShiftHistory.build(DList()), None)
      case Some(k) => run(+k)
    }

  def history(c: Cursor): ShiftHistory =
    run(c).history

  def cursor(c: Cursor): Option[Cursor] =
    run(c).cursor

  def or(c: Cursor): Cursor =
    cursor(c) getOrElse c

  def >->(k: Json => Json): Shift =
    withFocus(k)

  def withFocus(k: Json => Json): Shift =
    Shift.build(c => shift(c) map (r => ShiftResult(r.history, r.cursor map (_ >-> k))))

  def :=(q: => Json): Shift =
    set(q)

  def set(q: => Json): Shift =
    withFocus(_ => q)

  def compose(s: => Shift): Shift =
    Shift.build(c =>
      shift(c) flatMap (
        q => q.cursor match {
          case None => implicitly[Monad[Trampoline]].point(q)
          case Some(cc) => s shift cc map (r => ShiftResult(q.history ++ r.history, r.cursor))
        }))

  def andThen(s: Shift): Shift =
    s compose this

  def unary_~ : Shift =
    attempt

  def attempt: Shift =
    Shift.build(c =>
      shift(c) map (r => ShiftResult(r.history, r.cursor orElse Some(c)))
    )

  def %(n: Int): Shift =
    repeat(n)

  def repeat(n: Int): Shift = {
    @annotation.tailrec
    def go(n: Int, acc: Shift): Shift =
      if (n <= 0)
        acc
      else
        go(n - 1, this compose acc)

    go(n, this)
  }

  def left: Shift =
    this compose Shift.tramps(c => (ShiftLeft, c.left))

  def right: Shift =
    this compose Shift.tramps(c => (ShiftRight, c.right))

  def first: Shift =
    this compose Shift.tramps(c => (ShiftFirst, c.first))

  def last: Shift =
    this compose Shift.tramps(c => (ShiftLast, c.last))

  def -<-:(n: Int): Shift =
    leftN(n)

  def leftN(n: Int): Shift =
    this compose Shift.tramps(c => (ShiftLeftN(n), n -<-: c))

  def ->-:(n: Int): Shift =
    rightN(n)

  def rightN(n: Int): Shift =
    this compose Shift.tramps(c => (ShiftRightN(n), c :->- n))

  def ?<-:(p: Json => Boolean): Shift =
    leftAt(p)

  def leftAt(p: Json => Boolean): Shift =
    this compose Shift.tramps(c => (ShiftLeftAt(p), p ?<-: c))

  def :->?(p: Json => Boolean): Shift =
    rightAt(p)

  def rightAt(p: Json => Boolean): Shift =
    this compose Shift.tramps(c => (ShiftRightAt(p), c :->? p))

  def --(f: JsonField): Shift =
    field(f)

  def field(f: JsonField): Shift =
    this compose Shift.tramps(c => (ShiftField(f), c -- f))

  def --\(f: JsonField): Shift =
    downField(f)

  def downField(f: JsonField): Shift =
    this compose Shift.tramps(c => (ShiftDownField(f), c --\ f))

  def \\ : Shift =
    downArray

  def downArray: Shift =
    this compose Shift.tramps(c => (ShiftDownArray, c.downArray))

  def -\(p: Json => Boolean): Shift =
    downAt(p)

  def downAt(p: Json => Boolean): Shift =
    this compose Shift.tramps(c => (ShiftDownAt(p), c -\ p))

  def =\(n: Int): Shift =
    downN(n)

  def downN(n: Int): Shift =
    this compose Shift.tramps(c => (ShiftDownN(n), c =\ n))

  def delete: Shift =
    deleteGoParent

  def unary_! : Shift =
    deleteGoParent

  def deleteGoParent: Shift =
    this compose Shift.tramps(c => (ShiftDeleteGoParent, !c))

  def deleteGoLeft: Shift =
    this compose Shift.tramps(c => (ShiftDeleteGoLeft, c.deleteGoLeft))

  def deleteGoRight: Shift =
    this compose Shift.tramps(c => (ShiftDeleteGoRight, c.deleteGoRight))

  def deleteGoFirst: Shift =
    this compose Shift.tramps(c => (ShiftDeleteGoFirst, c.deleteGoFirst))

  def deleteGoLast: Shift =
    this compose Shift.tramps(c => (ShiftDeleteGoLast, c.deleteGoLast))

  def deleteGoField(f: JsonField): Shift =
    this compose Shift.tramps(c => (ShiftDeleteGoField(f), c.deleteGoField(f)))

  def up: Shift =
    this compose Shift.tramps(c => (ShiftUp, c.up))

}

object Shift extends Shifts {
  def apply(k: Cursor => Option[Cursor]): Shift =
    tramp(c => ShiftResult(ShiftHistory.build(DList()), k(c)))
}

trait Shifts {
  private[argonaut] def build(f: Cursor => Trampoline[ShiftResult]): Shift =
    new Shift {
      def shift(c: Cursor) = f(c)
    }

  private[argonaut] def tramp(f: Cursor => ShiftResult): Shift =
    build(c => implicitly[Monad[Trampoline]].point(f(c)))

  private[argonaut] def tramps(f: Cursor => (ShiftHistoryElement, Option[Cursor])): Shift =
    tramp(c => {
      val (e, q) = f(c)
      ShiftResult(ShiftHistory(e), q)
    })

  def shift: Shift =
    tramp(c => ShiftResult(ShiftHistory.build(DList()), Some(c)))

  implicit val ShiftInstances: Monoid[Shift] =
    new Monoid[Shift] {
      def append(s1: Shift, s2: => Shift): Shift =
        s1 compose s2
      def zero =
        shift
    }
}
