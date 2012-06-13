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

  def withFocus(k: Json => Json): Shift =
    Shift.build(c => shift(c) map (r => ShiftResult(r.history, r.cursor map (_ >-> k))))

  def >-->(k: Json => Json): Shift =
    withFocus(k)

  def set(q: => Json): Shift =
    withFocus(_ => q)

  def :=(q: => Json): Shift =
    set(q)

  def >=>(s: => Shift): Shift =
    Shift.build(c =>
      shift(c) flatMap (
        q => q.cursor match {
          case None => implicitly[Monad[Trampoline]].point(q)
          case Some(cc) => s shift cc map (r => ShiftResult(q.history ++ r.history, r.cursor))
        }))

  def <=<(s: Shift): Shift =
    s >=> this

  def attempt: Shift =
    Shift.build(c =>
      shift(c) map (r => ShiftResult(r.history, r.cursor orElse Some(c)))
    )

  def unary_~ : Shift =
    attempt

  def %(n: Int): Shift = {
    @annotation.tailrec
    def go(n: Int, acc: Shift): Shift =
      if (n <= 0)
        acc
      else
        go(n - 1, this >=> acc)

    go(n, this)
  }

  def left: Shift =
    this >=> Shift.tramps(c => (ShiftLeft, c.left))

  def right: Shift =
    this >=> Shift.tramps(c => (ShiftRight, c.right))

  def first: Shift =
    this >=> Shift.tramps(c => (ShiftFirst, c.first))

  def last: Shift =
    this >=> Shift.tramps(c => (ShiftLast, c.last))

  def up: Shift =
    this >=> Shift.tramps(c => (ShiftUp, c.up))

  def downArray: Shift =
    this >=> Shift.tramps(c => (ShiftDown, c.downArray))

  def -\(p: Json => Boolean): Shift =
    this >=> Shift.tramps(c => (ShiftDownAt(p), c -\ p))

  def =\(n: Int): Shift =
    this >=> Shift.tramps(c => (ShiftDownN(n), c =\ n))

  def ?<-:(p: Json => Boolean): Shift =
    this >=> Shift.tramps(c => (ShiftLeftAt(p), p ?<-: c))

  def :->?(p: Json => Boolean): Shift =
    this >=> Shift.tramps(c => (ShiftRightAt(p), c :->? p))

  def --(f: JsonField): Shift =
    this >=> Shift.tramps(c => (SiblingField(f), c -- f))

  def --\(f: JsonField): Shift =
    this >=> Shift.tramps(c => (DownField(f), c --\ f))

  def delete: Shift =
    this >=> Shift.tramps(c => (DeleteGoParent, c.deleteGoParent))

  def deleteGoParent: Shift =
    this >=> Shift.tramps(c => (DeleteGoParent, c.deleteGoParent))

  def deleteGoLeft: Shift =
    this >=> Shift.tramps(c => (DeleteGoLeft, c.deleteGoLeft))

  def deleteGoRight: Shift =
    this >=> Shift.tramps(c => (DeleteGoRight, c.deleteGoRight))

  def deleteGoFirst: Shift =
    this >=> Shift.tramps(c => (DeleteGoFirst, c.deleteGoFirst))

  def deleteGoLast: Shift =
    this >=> Shift.tramps(c => (DeleteGoLast, c.deleteGoLast))

  def deleteGoField(f: JsonField): Shift =
    this >=> Shift.tramps(c => (DeleteGoField(f), c.deleteGoField(f)))
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
        s1 >=> s2
      def zero =
        shift
    }
}
