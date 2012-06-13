package com.ephox
package argonaut

import scalaz._, Scalaz._

sealed trait PrettyParams {
  val lbraceLeft: JsonWhitespaces
  val lbraceRight: JsonWhitespaces
  val rbraceLeft: JsonWhitespaces
  val rbraceRight: JsonWhitespaces
  val lbracketLeft: JsonWhitespaces
  val lbracketRight: JsonWhitespaces
  val rbracketLeft: JsonWhitespaces
  val rbracketRight: JsonWhitespaces
  val commaLeft: JsonWhitespaces
  val commaRight: JsonWhitespaces
  val colonLeft: JsonWhitespaces
  val colonRight: JsonWhitespaces
  val indent: Int => JsonWhitespaces

  def pretty(j: Json): String =
    listpretty(j).toList.mkString

  def listpretty(j: Json): DList[Char] = {
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

    val lbrace = List(lbraceLeft.chars, DList('{'), lbraceRight.chars)
    val rbrace = List(rbraceLeft.chars, DList('}'), rbraceRight.chars)
    val lbracket = List(lbracketLeft.chars, DList('['), lbracketRight.chars)
    val rbracket = List(rbracketLeft.chars, DList(']'), rbracketRight.chars)
    val comma = List(commaLeft.chars, DList(','), commaRight.chars)
    val colon = List(colonLeft.chars, DList(':'), colonRight.chars)

    def trav(i: Int, k: Json): DList[Char] =
      indent(i).chars ++ k.fold(
        DList('n', 'u', 'l', 'l')
      , if(_) DList('t', 'r', 'u', 'e') else DList('f', 'a', 'l', 's', 'e')
      , n => DList.fromList((if(math.floor(n) == n && math.round(n).toDouble == n)
               math.round(n).toString
             else
               n.toString).toList)
      , s => '"' +: DList.fromList(s.toList flatMap escape) :+ '"'
      , e => {
          def spin(g: List[DList[Char]]): DList[Char] =
            g match {
              case Nil => rbracket.suml
              case h::t => h ++ spin(t)
            }
          lbracket.suml ++ spin(e map (trav(i + 1, _)) intercalate comma)
        }
      , o => {
          def spin(g: List[DList[Char]]): DList[Char] =
            g match {
              case Nil => rbrace.suml
              case h::t => h ++ spin(t)
            }
          lbrace.suml ++ spin(o.toList map {
            case (f, jj) => ('"' +: DList.fromList(f.toList flatMap escape) :+ '"') ++ colon.suml ++ trav(i + 1, jj)
          } intercalate comma)
        })

    trav(0, j)
  }
}

object PrettyParams extends PrettyParamss {
  def apply(
             lbraceLeft0: JsonWhitespaces
           , lbraceRight0: JsonWhitespaces
           , rbraceLeft0: JsonWhitespaces
           , rbraceRight0: JsonWhitespaces
           , lbracketLeft0: JsonWhitespaces
           , lbracketRight0: JsonWhitespaces
           , rbracketLeft0: JsonWhitespaces
           , rbracketRight0: JsonWhitespaces
           , commaLeft0: JsonWhitespaces
           , commaRight0: JsonWhitespaces
           , colonLeft0: JsonWhitespaces
           , colonRight0: JsonWhitespaces
           , indent0: Int => JsonWhitespaces
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
      val indent = indent0
    }
}

trait PrettyParamss {
  def compact: PrettyParams =
    PrettyParams(
      Monoid[JsonWhitespaces].zero
    , Monoid[JsonWhitespaces].zero
    , Monoid[JsonWhitespaces].zero
    , Monoid[JsonWhitespaces].zero
    , Monoid[JsonWhitespaces].zero
    , Monoid[JsonWhitespaces].zero
    , Monoid[JsonWhitespaces].zero
    , Monoid[JsonWhitespaces].zero
    , Monoid[JsonWhitespaces].zero
    , Monoid[JsonWhitespaces].zero
    , Monoid[JsonWhitespaces].zero
    , Monoid[JsonWhitespaces].zero
    , (_: Int) => Monoid[JsonWhitespaces].zero
    )

  def pretty: PrettyParams =
    PrettyParams(
      Monoid[JsonWhitespaces].zero
    , +JsonLine
    , +JsonLine
    , Monoid[JsonWhitespaces].zero
    , Monoid[JsonWhitespaces].zero
    , Monoid[JsonWhitespaces].zero
    , Monoid[JsonWhitespaces].zero
    , Monoid[JsonWhitespaces].zero
    , Monoid[JsonWhitespaces].zero
    , +JsonLine
    , Monoid[JsonWhitespaces].zero
    , Monoid[JsonWhitespaces].zero
    , (n: Int) => JsonSpace * (2 * n)
    )
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

    go(n, JsonWhitespaces.build(DList()))
  }

  def unary_+ : JsonWhitespaces =
    JsonWhitespaces(this)
}
case object JsonSpace extends JsonWhitespace
case object JsonTab extends JsonWhitespace
case object JsonLine extends JsonWhitespace
case object JsonReturn extends JsonWhitespace

sealed trait JsonWhitespaces {
  val value: DList[JsonWhitespace]

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
    go(n, JsonWhitespaces.build(DList()))
  }
  def toList: List[JsonWhitespace] =
    value.toList

  def string: String =
    toList map (_.toChar) mkString

  def chars: DList[Char] =
    value map (_.toChar)

  def lbraceLeftL: PrettyParams @> JsonWhitespaces =
    Lens(p => Costate(PrettyParams(_, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight, p.indent), p.lbraceLeft))

  def lbraceRightL: PrettyParams @> JsonWhitespaces =
    Lens(p => Costate(PrettyParams(p.lbraceLeft, _, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight, p.indent), p.lbraceRight))

  def rbraceLeftL: PrettyParams @> JsonWhitespaces =
    Lens(p => Costate(PrettyParams(p.lbraceLeft, p.lbraceRight, _, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight, p.indent), p.rbraceLeft))

  def rbraceRightL: PrettyParams @> JsonWhitespaces =
    Lens(p => Costate(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, _, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight, p.indent), p.rbraceRight))

  def lbracketLeftL: PrettyParams @> JsonWhitespaces =
    Lens(p => Costate(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, _, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight, p.indent), p.lbracketLeft))

  def lbracketRightL: PrettyParams @> JsonWhitespaces =
    Lens(p => Costate(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, _, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight, p.indent), p.lbracketRight))

  def rbracketLeftL: PrettyParams @> JsonWhitespaces =
    Lens(p => Costate(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, _, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight, p.indent), p.rbracketLeft))

  def rbracketRightL: PrettyParams @> JsonWhitespaces =
    Lens(p => Costate(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, _, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight, p.indent), p.rbracketRight))

  def commaLeftL: PrettyParams @> JsonWhitespaces =
    Lens(p => Costate(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, _, p.commaRight, p.colonLeft, p.colonRight, p.indent), p.commaLeft))

  def commaRightL: PrettyParams @> JsonWhitespaces =
    Lens(p => Costate(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, _, p.colonLeft, p.colonRight, p.indent), p.commaRight))

  def colonLeftL: PrettyParams @> JsonWhitespaces =
    Lens(p => Costate(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, _, p.colonRight, p.indent), p.colonLeft))

  def colonRightL: PrettyParams @> JsonWhitespaces =
    Lens(p => Costate(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, _, p.indent), p.colonRight))

  def indentL: PrettyParams @> (Int => JsonWhitespaces) =
    Lens(p => Costate(PrettyParams(p.lbraceLeft, p.lbraceRight, p.rbraceLeft, p.rbraceRight, p.lbracketLeft, p.lbracketRight, p.rbracketLeft, p.rbracketRight, p.commaLeft, p.commaRight, p.colonLeft, p.colonRight, _), p.indent))
}

object JsonWhitespaces extends JsonWhitespacess {
  def apply(v: JsonWhitespace*): JsonWhitespaces =
    build(DList.fromList(v.toList))

  private[argonaut] def build(v: DList[JsonWhitespace]): JsonWhitespaces =
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
        JsonWhitespaces.build(DList())
      def append(s1: JsonWhitespaces, s2: => JsonWhitespaces) =
        s1 ++ s2
    }

}

object Y {
  def main(args: Array[String]) {
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

    r foreach (j => println(PrettyParams.pretty.pretty(j)))

    // PrettyParams.compact.pretty()
    println("hi")
  }
}