package guide.chapter4

import shapeless.ops.hlist.{IsHCons, Last}
import shapeless.{::, Generic, HList, HNil}

trait Second[L <: HList] {
  type Out

  def apply(value: L): Out
}

object Second {
  type Aux[L <: HList, O] = Second[L] {type Out = O}

  def apply[L <: HList](implicit inst: Second[L]): Aux[L, inst.Out] = inst

  implicit def hlistSecond[F, S, Rest <: HList]: Aux[F :: S :: Rest, S] = new Second[F :: S :: Rest] {
    override type Out = S

    override def apply(value: F :: S :: Rest): S = value.tail.head
  }
}

final case class Bar(name: String, num: Int)

final case class Foo(bar: Bar, num: Double, age: Int)

object TypesAndImplicits extends App {
  val last1 = Last[String :: Int :: HNil]
  val num: Int = last1("foo" :: 123 :: HNil)

  val second1 = Second[String :: Boolean :: Int :: HNil]
  val bool: Boolean = second1("bar" :: true :: 1 :: HNil)

  def lastField[A, R <: HList](input: A)(implicit
                                         gen: Generic.Aux[A, R],
                                         last: Last[R]
  ): last.Out = last(gen.to(input))

  lastField(Bar("bar", 1))
  lastField(Foo(Bar("bar", 1), 1.0d, 1))

  def getWrappedValue[A, Repr <: HList, Head](input: A)(
    implicit
    gen: Generic.Aux[A, Repr],
    isHCons: IsHCons.Aux[Repr, Head, HNil]
  ): Head = gen.to(input).head

  case class Wrapper(value: Int)

  getWrappedValue(Wrapper(42))
}
