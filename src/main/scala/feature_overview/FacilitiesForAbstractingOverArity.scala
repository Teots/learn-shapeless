package feature_overview

import shapeless.{Generic, HList}
import shapeless.syntax.std.function._
import shapeless.ops.function._

// Supporting conversion of arbitrary functions to functions of a single `HList` argument.
object FacilitiesForAbstractingOverArity extends App {

  def applyProduct[P <: Product, F, L <: HList, R](p: P)(f: F)(implicit gen: Generic.Aux[P, L], fp: FnToProduct.Aux[F, L => R]) =
    f.toProduct(gen.to(p))

  println(applyProduct(1, 2)((_: Int)+(_: Int)))
  println(applyProduct(1, 2, 3)((_: Int)*(_: Int)*(_: Int)))
}
