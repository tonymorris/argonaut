package com.ephox
package argonaut

import scalaz._, Scalaz._, Free._
import Json._

sealed trait Shift {
  private[argonaut] def shift(c: Cursor): Trampoline[(ShiftHistory, Option[Cursor])]

  def run(c: Cursor): (ShiftHistory, Option[Cursor]) =
    shift(c).run

  // todo delete
  def runj(j: Option[Json]): (ShiftHistory, Option[Cursor]) =
    j match {
      case None => (ShiftHistory.build(DList()), None)
      case Some(k) => run(+k)
    }

  def history(c: Cursor): ShiftHistory =
    run(c)._1

  def cursor(c: Cursor): Option[Cursor] =
    run(c)._2

  def cursorOr(c: Cursor, d: => Cursor): Cursor =
    cursor(c) getOrElse d

  def or(c: Cursor): Cursor =
    cursor(c) getOrElse c

  def withFocus(k: Json => Json): Shift =
    Shift.build(c => shift(c) map {
      case (h, d) => (h, d map (_ >-> k))
    })

  def >-->(k: Json => Json): Shift =
    withFocus(k)

  def set(q: => Json): Shift =
    withFocus(_ => q)

  def :=(q: => Json): Shift =
    set(q)

  def >=>(s: => Shift): Shift =
    Shift.build(c =>
      shift(c) flatMap {
        case q@(w, d) => d match {
          case None => implicitly[Monad[Trampoline]].point(q)
          case Some(cc) => s shift cc map {
            case (ww, ccc) => (w ++ ww, ccc)
          }
        }
      })

  def <=<(s: Shift): Shift =
    s >=> this

  def attempt: Shift =
    Shift.build(c =>
      shift(c) map {
        case (w, d) => (w, d orElse Some(c))
      })

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
    tramp(c => (ShiftHistory.build(DList()), k(c)))
}

trait Shifts {
  private[argonaut] def build(f: Cursor => Trampoline[(ShiftHistory, Option[Cursor])]): Shift =
    new Shift {
      def shift(c: Cursor) = f(c)
    }

  private[argonaut] def tramp(f: Cursor => (ShiftHistory, Option[Cursor])): Shift =
    build(c => implicitly[Monad[Trampoline]].point(f(c)))

  private[argonaut] def tramps(f: Cursor => (ShiftHistoryElement, Option[Cursor])): Shift =
    tramp(c => {
      val (e, q) = f(c)
      (ShiftHistory(e), q)
    })

  def shiftId: Shift =
    tramp(c => (ShiftHistory.build(DList()), Some(c)))

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

  implicit val ShiftHistoryInstances: Show[ShiftHistory] =
    new Show[ShiftHistory] {
      def show(h: ShiftHistory) = Show[List[ShiftHistoryElement]].show(h.toList.toList)
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
  implicit val ShiftHistoryElementInstances: Show[ShiftHistoryElement] =
    new Show[ShiftHistoryElement] {
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
    }
}