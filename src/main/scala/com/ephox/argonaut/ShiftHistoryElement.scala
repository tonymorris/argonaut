package com.ephox
package argonaut

import Json._
import scalaz._, Scalaz._

sealed trait ShiftHistoryElement {

}
case object ShiftLeft extends ShiftHistoryElement
case object ShiftRight extends ShiftHistoryElement
case object ShiftFirst extends ShiftHistoryElement
case object ShiftLast extends ShiftHistoryElement
case object ShiftUp extends ShiftHistoryElement
case object ShiftDown extends ShiftHistoryElement
case class ShiftDownAt(p: Json => Boolean) extends ShiftHistoryElement
case class ShiftDownN(n: Int) extends ShiftHistoryElement
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
          case ShiftDown => "\\\\"
          case ShiftDownAt(_) => "-\\"
          case ShiftDownN(n) => "-\\(" + n + ")"
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