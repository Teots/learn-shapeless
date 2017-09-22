package feature_overview

import shapeless.TypeCase
import shapeless.syntax.typeable._

object TypeSafeCast extends App {
  val l: Any = List(Vector("foo", "bar", "baz"), Vector("wibble"))

  println(l.cast[List[Vector[String]]])
  println(l.cast[List[Vector[Int]]])
  println(l.cast[List[Vector[String]]])

  val `List[String]` = TypeCase[List[String]]
  val `List[Int]` = TypeCase[List[Int]]

  val nl = List(1, 2, 3)
  val res = (nl: Any) match {
    case `List[String]`(List(s, _*)) => s.length
    case `List[Int]`(List(i, _*)) => i + 1
  }

  println(res) // 2
}
