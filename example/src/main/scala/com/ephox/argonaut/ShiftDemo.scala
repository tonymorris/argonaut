package com.ephox
package argonaut

import Argonaut._
import scalaz._, Scalaz._

object ShiftDemo {
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

    val r = shift.--\("values").downArray.downArray.right.>-->(jStringL =>= (_.reverse)).up.right := jString("cat")
    val s = r <| j.pparse
    s.cursor map (c => JsonPrinter.pretty(-c)) foreach println
    s.println
  }
}
