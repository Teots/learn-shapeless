package feature_overview

import shapeless.Poly1
import shapeless.PolyDefns.~>

// choose is a function from Sets to Options with no type specific cases
object choose extends (Set ~> Option) {
  def apply[T](s: Set[T]) = s.headOption
}

object size extends Poly1 {
  implicit def caseInt = at[Int](_ => 1)
  implicit def caseString = at[String](_.length)
  implicit def caseTuple[T, U](implicit st: Case.Aux[T, Int], su: Case.Aux[U, Int]) = at[(T, U)](t => size(t._1) + size(t._2))
}

object PolymorphicFunctionValues extends App {
  // Normal scala functions are monomorphic and thus the following code won't compile
  // val f = { x => x }
  // val tpl: (Int, Double) = (f(5), f(5.0))

  choose(Set(1, 2, 3))
  choose(Set('a', 'b', 'c'))
  choose(Set.empty[String])

  size(23) // 1
  size("foo") // 3
  size((23, "foo")) // 1 + 3 = 4
}
