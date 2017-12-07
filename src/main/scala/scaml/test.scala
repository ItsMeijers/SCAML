package scaml

import atto._, Atto._
import cats.implicits._
import parser.YAMLParser._

object test extends App {

  println(int.parse("123").done)

  println(Float.NaN)
  println(Float.PositiveInfinity)
  println(Float.NegativeInfinity)

  println(blockComment.parse("#testing one to tree\n dafdsfs").done)

  println(namedTagHandle.parse("!testing-this !asdfasd").done)

}
