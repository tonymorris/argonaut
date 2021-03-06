package argonaut.doc

import argonaut._, Argonaut._

object CustomPrinters extends App {

  val json = ("red" := true) ->: ("blue" := false) ->: jEmptyObject

  val custom = colonLeftL.set(spaces2, _ => JsonWhitespaces())

  println(json.pretty(custom))

}
