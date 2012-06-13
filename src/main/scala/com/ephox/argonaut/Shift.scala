package com.ephox
package argonaut

import scalaz._, Scalaz._, Free._
import Json._

sealed trait Shift {
  private[argonaut] def shift(c: Cursor): Trampoline[ShiftResult]

  def run(c: Cursor): ShiftResult =
    shift(c).run

  // todo delete
  def |>(j: Option[Json]): ShiftResult =
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

  def shiftId: Shift =
    tramp(c => ShiftResult(ShiftHistory.build(DList()), Some(c)))

  implicit val ShiftInstances: Monoid[Shift] =
    new Monoid[Shift] {
      def append(s1: Shift, s2: => Shift): Shift =
        s1 >=> s2
      def zero =
        shiftId
    }

  object Shift {
    def left: Shift =
      tramps(c => (ShiftLeft, c.left))

    def right: Shift =
      tramps(c => (ShiftRight, c.right))

    def first: Shift =
      tramps(c => (ShiftFirst, c.first))

    def last: Shift =
      tramps(c => (ShiftLast, c.last))

    def up: Shift =
      tramps(c => (ShiftUp, c.up))

    def down: Shift =
      tramps(c => (ShiftDown, c.downArray))

    def leftAt(p: Json => Boolean): Shift =
      tramps(c => (ShiftLeftAt(p), c :->? p))

    def rightAt(p: Json => Boolean): Shift =
      tramps(c => (ShiftRightAt(p), p ?<-: c))

    def field(f: JsonField): Shift =
      tramps(c => (SiblingField(f), c -- f))

    def downField(f: JsonField): Shift =
      tramps(c => (DownField(f), c --\ f))

    def delete: Shift =
      tramps(c => (DeleteGoParent, c.deleteGoParent))

    def deleteGoParent: Shift =
      tramps(c => (DeleteGoParent, c.deleteGoParent))

    def deleteGoLeft: Shift =
      tramps(c => (DeleteGoLeft, c.deleteGoLeft))

    def deleteGoRight: Shift =
      tramps(c => (DeleteGoRight, c.deleteGoRight))

    def deleteGoFirst: Shift =
      tramps(c => (DeleteGoFirst, c.deleteGoFirst))

    def deleteGoLast: Shift =
      tramps(c => (DeleteGoLast, c.deleteGoLast))

    def deleteGoField(f: JsonField): Shift =
      tramps(c => (DeleteGoField(f), c.deleteGoField(f)))
  }
}

////

sealed trait ShiftHistory {
  val toList: DList[ShiftHistoryElement]

  def ++(h: ShiftHistory): ShiftHistory =
    ShiftHistory.build(toList ++ h.toList)

}

object ShiftHistory extends ShiftHistorys {
  private[argonaut] def apply(e: ShiftHistoryElement) =
    build(DList(e))
}

trait ShiftHistorys {
  private[argonaut] def build(l: DList[ShiftHistoryElement]): ShiftHistory =
    new ShiftHistory {
      val toList = l
    }

  implicit val ShiftHistoryInstances: Show[ShiftHistory] with Equal[ShiftHistory] with Monoid[ShiftHistory] =
    new Show[ShiftHistory] with Equal[ShiftHistory] with Monoid[ShiftHistory] {
      def show(h: ShiftHistory) = Show[List[ShiftHistoryElement]].show(h.toList.toList)
      def equal(h1: ShiftHistory, h2: ShiftHistory) =
        h1.toList === h2.toList
      def zero = ShiftHistory.build(DList())
      def append(h1: ShiftHistory, h2: => ShiftHistory) =
        h1 ++ h2
    }
}

////
sealed trait ShiftHistoryElement {

}
case object ShiftLeft extends ShiftHistoryElement
case object ShiftRight extends ShiftHistoryElement
case object ShiftFirst extends ShiftHistoryElement
case object ShiftLast extends ShiftHistoryElement
case object ShiftUp extends ShiftHistoryElement
case object ShiftDown extends ShiftHistoryElement
case class ShiftLeftAt(p: Json => Boolean) extends ShiftHistoryElement
case class ShiftRightAt(p: Json => Boolean) extends ShiftHistoryElement
case class SiblingField(f: JsonField) extends ShiftHistoryElement
case class DownField(f: JsonField) extends ShiftHistoryElement
case object DeleteGoParent extends ShiftHistoryElement
case object DeleteGoLeft extends ShiftHistoryElement
case object DeleteGoRight extends ShiftHistoryElement
case object DeleteGoFirst extends ShiftHistoryElement
case object DeleteGoLast extends ShiftHistoryElement
case class DeleteGoField(f: JsonField) extends ShiftHistoryElement

object ShiftHistoryElement extends ShiftHistoryElements

trait ShiftHistoryElements {
  implicit val ShiftHistoryElementInstances: Show[ShiftHistoryElement] with Equal[ShiftHistoryElement] =
    new Show[ShiftHistoryElement] with Equal[ShiftHistoryElement] {
      def show(e: ShiftHistoryElement) =
        (e match {
          case ShiftLeft => "<-"
          case ShiftRight => "->"
          case ShiftFirst => "|<-"
          case ShiftLast => "->|"
          case ShiftUp => "_/"
          case ShiftDown => "-\\"
          case ShiftLeftAt(_) => "<?-"
          case ShiftRightAt(_) => "-?>"
          case SiblingField(f) => "--(" + f + ")"
          case DownField(f) => "--\\(" + f + ")"
          case DeleteGoParent => "!_/"
          case DeleteGoLeft => "<-!"
          case DeleteGoRight => "!->"
          case DeleteGoFirst => "|<-!"
          case DeleteGoLast => "!->|"
          case DeleteGoField(f) => "!--(" + f + ")"
        }).toList

      def equal(e1: ShiftHistoryElement, e2: ShiftHistoryElement) =
        e1 == e2
    }
}

sealed trait ShiftResult {
  val history: ShiftHistory
  val cursor: Option[Cursor]

  def hasCursor =
    cursor.isDefined

  def cursorOr(c: => Cursor): Cursor =
    cursor getOrElse c
}

object ShiftResult extends ShiftResults {
  def apply(h: ShiftHistory, c: Option[Cursor]): ShiftResult =
    new ShiftResult {
      val history = h
      val cursor = c
    }
}

trait ShiftResults {
  implicit val ShiftResultInstances: Equal[ShiftResult] with Show[ShiftResult] =
    new Equal[ShiftResult] with Show[ShiftResult] {
      def equal(r1: ShiftResult, r2: ShiftResult) =
        Equal.equalBy((r: ShiftResult) => (r.history, r.cursor)).equal(r1, r2)
      def show(r: ShiftResult) =
        ("{ history=" + r.history.shows + (r.cursor match {
          case None => ""
          case Some(c) => ", cursor=" + c.shows
        }) + " }").toList
    }

  def resultHistoryL: ShiftResult @> ShiftHistory =
    Lens(r => Costate(ShiftResult(_, r.cursor), r.history))

  def resultCursorL: ShiftResult @> Option[Cursor] =
    Lens(r => Costate(ShiftResult(r.history, _), r.cursor))

  def resultCursorPL: ShiftResult @?> Cursor =
    PLensT.somePLens compose ~resultCursorL

}