package com.ephox
package argonaut

import scalaz._, Scalaz._

sealed trait PrettyParams {
  val lbraceLeft: Int => JsonWhitespaces
  val lbraceRight: Int => JsonWhitespaces
  val rbraceLeft: Int => JsonWhitespaces
  val rbraceRight: Int => JsonWhitespaces
  val lbracketLeft: Int => JsonWhitespaces
  val lbracketRight: Int => JsonWhitespaces
  val rbracketLeft: Int => JsonWhitespaces
  val rbracketRight: Int => JsonWhitespaces
  val commaLeft: Int => JsonWhitespaces
  val commaRight: Int => JsonWhitespaces
  val colonLeft: Int => JsonWhitespaces
  val colonRight: Int => JsonWhitespaces

  def pretty(j: Json): String =
    lpretty(j).toList.mkString

  def lpretty(j: Json): Vector[Char] = {
    def escape(c: Char): List[Char] =
      c match {
        case '\\' => List('\\', '\\')
        case '"' => List('\\', '"')
        case '\b' => List('\\', 'b')
        case '\f' => List('\\', 'f')
        case '\n' => List('\\', 'n')
        case '\r' => List('\\', 'r')
        case '\t' => List('\\', 't')
        case _ => List(c)
      }

    def trav(depth: Int, k: Json): Vector[Char] = {
      val lbrace = List(lbraceLeft(depth).chars, Vector('{'), lbraceRight(depth).chars)
      val rbrace = List(rbraceLeft(depth).chars, Vector('}'), rbraceRight(depth).chars)
      val lbracket = List(lbracketLeft(depth).chars, Vector('['), lbracketRight(depth).chars)
      val rbracket = List(rbracketLeft(depth).chars, Vector(']'), rbracketRight(depth).chars)
      val comma = List(commaLeft(depth).chars, Vector(','), commaRight(depth).chars)
      val colon = List(colonLeft(depth).chars, Vector(':'), colonRight(depth).chars)

      k.fold(
        Vector('n', 'u', 'l', 'l')
      , if(_) Vector('t', 'r', 'u', 'e') else Vector('f', 'a', 'l', 's', 'e')
      , n => Vector.fromList((if(math.floor(n) == n && math.round(n).toDouble == n)
               math.round(n).toString
             else
               n.toString).toList)
      , s => error("") // '"' +: DList.fromList(s.toList flatMap escape) :+ '"'
      , e => error("") /* {
          def spin(g: List[Vector[Char]]): Vector[Char] =
            g match {
              case Nil => rbracket.suml
              case h::t => h ++ spin(t)
            }
          lbracket.suml ++ spin(e map (trav(depth + 1, _)) intercalate comma)
        }                 */
      , o => error("") /*{
          def spin(g: List[Vector[Char]]): Vector[Char] =
            g match {
              case Nil => rbrace.suml
              case h::t => h ++ spin(t)
            }
          lbrace.suml ++ spin(o.toList map {
            case (f, jj) => ('"' +: Vector.fromList(f.toList flatMap escape) :+ '"') ++ colon.suml ++ trav(depth + 1, jj)
          } intercalate comma)
        }*/)
    }

    trav(0, j)
  }
}

object PrettyParams extends PrettyParamss {
  def apply(
             lbraceLeft0: Int => JsonWhitespaces
           , lbraceRight0: Int => JsonWhitespaces
           , rbraceLeft0: Int => JsonWhitespaces
           , rbraceRight0: Int => JsonWhitespaces
           , lbracketLeft0: Int => JsonWhitespaces
           , lbracketRight0: Int => JsonWhitespaces
           , rbracketLeft0: Int => JsonWhitespaces
           , rbracketRight0: Int => JsonWhitespaces
           , commaLeft0: Int => JsonWhitespaces
           , commaRight0: Int => JsonWhitespaces
           , colonLeft0: Int => JsonWhitespaces
           , colonRight0: Int => JsonWhitespaces
           ): PrettyParams =
    new PrettyParams {
      val lbraceLeft = lbraceLeft0
      val lbraceRight = lbraceRight0
      val rbraceLeft = rbraceLeft0
      val rbraceRight = rbraceRight0
      val lbracketLeft = lbracketLeft0
      val lbracketRight = lbracketRight0
      val rbracketLeft = rbracketLeft0
      val rbracketRight = rbracketRight0
      val commaLeft = commaLeft0
      val commaRight = commaRight0
      val colonLeft = colonLeft0
      val colonRight = colonRight0
    }
}

trait PrettyParamss {
  def compact: PrettyParams =
    PrettyParams(
      _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
    )

  def pretty(indent: JsonWhitespaces): PrettyParams = {
    PrettyParams(
      _ => Monoid[JsonWhitespaces].zero
    , n => {
        println(n)
        indent * 3
      }
    , _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
    , _ => Monoid[JsonWhitespaces].zero
    )
  }

  def spaces2 = pretty(JsonSpace * 2): PrettyParams
}

sealed trait JsonWhitespace {
  def toChar: Char =
    this match {
      case JsonSpace => ' ' // %x20
      case JsonTab => '\t' // %x09
      case JsonLine => '\n' // %x0A
      case JsonReturn => '\r' // %x0D
    }

  def *(n: Int): JsonWhitespaces = {
    @annotation.tailrec
    def go(x: Int, w: JsonWhitespaces): JsonWhitespaces =
      if(x <= 0)
        w
      else
        go(x - 1, this +: w)

    go(n, JsonWhitespaces.build(Vector()))
  }

  def unary_+ : JsonWhitespaces =
    JsonWhitespaces(this)
}
case object JsonSpace extends JsonWhitespace
case object JsonTab extends JsonWhitespace
case object JsonLine extends JsonWhitespace
case object JsonReturn extends JsonWhitespace

sealed trait JsonWhitespaces {
  val value: Vector[JsonWhitespace]

  def +:(s: JsonWhitespace): JsonWhitespaces =
    JsonWhitespaces.build(s +: value)

  def :+(s: JsonWhitespace): JsonWhitespaces =
    JsonWhitespaces.build(value :+ s)

  def ++(s: JsonWhitespaces): JsonWhitespaces =
    JsonWhitespaces.build(value ++ s.value)

  def *(n: Int): JsonWhitespaces = {
    @annotation.tailrec
    def go(x: Int, w: JsonWhitespaces): JsonWhitespaces =
      if(x <= 0)
        w
      else
        go(x - 1, w ++ w)
    go(n, JsonWhitespaces.build(Vector()))
  }

  def toList: List[JsonWhitespace] =
    value.toList

  def string: String =
    value map (_.toChar) mkString

  def chars: Vector[Char] =
    value map (_.toChar)

  def lbraceLeftL: PrettyParams @> (Int => JsonWhitespaces) =
    Lens(p => Costate(PrettyParams(_, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight), p.lbraceLeft))

  def lbraceRightL: PrettyParams @> (Int => JsonWhitespaces) =
    Lens(p => Costate(PrettyParams(p.lbraceLeft, _, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight), p.lbraceRight))

  def rbraceLeftL: PrettyParams @> (Int => JsonWhitespaces) =
    Lens(p => Costate(PrettyParams(p.lbraceLeft, p.lbraceRight, _, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight), p.rbraceLeft))

  def rbraceRightL: PrettyParams @> (Int => JsonWhitespaces) =
    Lens(p => Costate(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, _, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight), p.rbraceRight))

  def lbracketLeftL: PrettyParams @> (Int => JsonWhitespaces) =
    Lens(p => Costate(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, _, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight), p.lbracketLeft))

  def lbracketRightL: PrettyParams @> (Int => JsonWhitespaces) =
    Lens(p => Costate(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, _, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight), p.lbracketRight))

  def rbracketLeftL: PrettyParams @> (Int => JsonWhitespaces) =
    Lens(p => Costate(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, _, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight), p.rbracketLeft))

  def rbracketRightL: PrettyParams @> (Int => JsonWhitespaces) =
    Lens(p => Costate(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, _, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight), p.rbracketRight))

  def commaLeftL: PrettyParams @> (Int => JsonWhitespaces) =
    Lens(p => Costate(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, _, p.commaRight, p.colonLeft, p.colonRight), p.commaLeft))

  def commaRightL: PrettyParams @> (Int => JsonWhitespaces) =
    Lens(p => Costate(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, _, p.colonLeft, p.colonRight), p.commaRight))

  def colonLeftL: PrettyParams @> (Int => JsonWhitespaces) =
    Lens(p => Costate(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, _, p.colonRight), p.colonLeft))

  def colonRightL: PrettyParams @> (Int => JsonWhitespaces) =
    Lens(p => Costate(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, _), p.colonRight))
}

object JsonWhitespaces extends JsonWhitespacess {
  def apply(v: JsonWhitespace*): JsonWhitespaces =
    build(Vector(v: _*))

  private[argonaut] def build(v: Vector[JsonWhitespace]): JsonWhitespaces =
    new JsonWhitespaces {
      val value = v
    }
}

trait JsonWhitespacess {
  implicit val JsonWhitespacesInstances: Equal[JsonWhitespaces] with Show[JsonWhitespaces] with Monoid[JsonWhitespaces] =
    new Equal[JsonWhitespaces] with Show[JsonWhitespaces] with Monoid[JsonWhitespaces] {
      def equal(s1: JsonWhitespaces, s2: JsonWhitespaces) =
        s1.toList == s2.toList
      def show(s: JsonWhitespaces) =
        s.toList map (_.toChar)
      def zero =
        JsonWhitespaces.build(Vector())
      def append(s1: JsonWhitespaces, s2: => JsonWhitespaces) =
        s1 ++ s2
    }

}

object Y {
  def main(args: Array[String]) {
    /*
    val j =
      """
        {
          "abc" :
            {
              "def" : 7
            },
          "ghi" :
            {
              "ata" : null,
              "jkl" :
                {
                  "mno" : "argo"
                }
            },
          "pqr" : false,
          "operator": "is",
          "values": [
                      [
                        "horse"
                      , "lolo"
                      , [
                          "hi"
                        , "there"
                        ]
                      ]
                    , "dog"
                    , "rabbit"
                    ],
          "xyz" : 24
        }
      """

    import StringWrap._
    val r = j.pparse

    r foreach (j => println(PrettyParams.spaces2.pretty(j)))

    // PrettyParams.compact.pretty()
    */
    println("hi")
  }
}
