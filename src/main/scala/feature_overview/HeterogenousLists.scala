package feature_overview

import shapeless.{HNil, ::, Poly2}
import shapeless.syntax.zipper._

object addSize extends Poly2 {
  implicit def default[T](implicit st: size.Case.Aux[T, Int]) = at[Int, T]{ (acc, t) => acc + size(t) }
}

class HeterogenousLists extends App {
  val l = 23 :: "foo" :: (13, "wibble") :: HNil
  l.foldLeft(0)(addSize) // 1 + 3 + (1 + 6) = 11

  // Create a zipper for the HList move the cursor to the right and get back an HList in the end
  val nl = 1 :: "foo" :: 3.0 :: HNil
  nl.toZipper.right.put(("wibble", 45)).reify

  // Covariance
  trait Fruit
  case class Apple() extends Fruit
  case class Pear() extends Fruit

  type FFFF = Fruit :: Fruit :: Fruit :: Fruit :: HNil
  type APAP = Apple :: Pear :: Apple :: Pear :: HNil

  val a : Apple = Apple()
  val p : Pear = Pear()

  val apap : APAP = a :: p :: a :: p :: HNil
  val ffff : FFFF = apap  // APAP <: FFFF
}
