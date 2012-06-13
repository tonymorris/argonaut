package com.ephox
package argonaut

import Argonaut._
import scalaz._, Scalaz._

object ShiftDemo {
  def main(args: Array[String]) {
    import Shift.Shift._

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
                        "cat"
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

    val q = j.pparse
    val r = downField("values") >=> down >=> down >=> right >--> (jStringL =>= (_.reverse)) >=> up >=> right := jString("cat")
    val (s, t) = r runj q
    val u = t map (c => JsonPrinter.pretty(-c))

    u.println
    s.println
  }
}
