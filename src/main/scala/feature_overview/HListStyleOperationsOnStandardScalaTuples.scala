package feature_overview

import shapeless.Id
import shapeless.poly._
import shapeless.syntax.std.tuple._
import shapeless.syntax.zipper._

object HListStyleOperationsOnStandardScalaTuples extends App {
  println((23, "foo", true).head)
  println((23, "foo", true).tail)
  println((23, "foo", true).drop(2))
  println((23, "foo", true).take(2))
  println((23, "foo", true).split(1))

  // prepend, append, concatenate
  println(23 +: ("foo", true))
  println((23, "foo") :+ true)
  println((23, "foo") ++ (true, 2.0))

  // map, flatMap, fold
  object option extends (Id ~> Option) {
    def apply[T](t: T) = Option(t)
  }

  println((23, "foo", true) map option)
  println(((23, "foo"), (), (true, 2.0)) flatMap identity)
  println((23, "foo", (13, "wibble")).foldLeft(0)(addSize))

  // conversion to `HList`s and ordinary Scala `List`s
  println((23, "foo", true).productElements)
  println((23, "foo", true).toList)
  println((23, ("foo", true), 2.0).toZipper.right.down.put("bar").root.reify)
}
