package feature_overview

import shapeless._, syntax.singleton._
import shapeless._
import syntax.singleton._

object SingletonTypedLiterals extends App {

  23.narrow
  "foo".narrow
  val (wTrue, wFalse) = (Witness(true), Witness(false))

  type True = wTrue.T
  type False = wFalse.T

  trait Select[B] { type Out }

  implicit val selInt = new Select[True] { type Out = Int }
  implicit val selString = new Select[False] { type Out = String }

  def select(b: WitnessWith[Select])(t: b.instance.Out) = t

  select(true)(23)

  // select(true)("foo") // Required `Int`
  // select(false)(23) // Required `String`

  select(false)("foo")

  'foo // non-singleton type
  'foo.narrow // singleton type
}
