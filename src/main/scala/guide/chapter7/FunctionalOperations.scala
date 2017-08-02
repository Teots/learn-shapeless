package guide.chapter7

import shapeless._
import shapeless.ops.hlist.Mapper

object myPoly extends Poly1 {
  implicit val intCase: Case.Aux[Int, Double] = at(num => num / 2.0)

  implicit val stringCase: Case.Aux[String, Int] = at(str => str.length)
}

object multiply extends Poly2 {
  implicit val intIntCase: Case.Aux[Int, Int, Int] = at((a, b) => a * b)
  implicit val intStrCase: Case.Aux[Int, String, String] = at((a, b) => b.toString * a)
}

object total extends Poly1 {
  implicit def base[A](implicit num: Numeric[A]): Case.Aux[A, Double] = at(num.toDouble)

  implicit def option[A](implicit num: Numeric[A]): Case.Aux[Option[A], Double] = at(opt => opt.fold(0.0d)(num.toDouble))

  implicit def list[A](implicit num: Numeric[A]): Case.Aux[List[A], Double] = at(list => list.map(num.toDouble).sum)
}

object sizeOf extends Poly1 {
  implicit val intCase: Case.Aux[Int, Int] = at(identity)
  implicit val stringCase: Case.Aux[String, Int] = at(_.length)
  implicit val booleanCase: Case.Aux[Boolean, Int] = at(bool => if (bool) 1 else 0)
}

object valueAndSizeOf extends Poly1 {
  implicit val intCase: Case.Aux[Int, Int :: Int :: HNil] = at(num => num :: num :: HNil)
  implicit val stringCase: Case.Aux[String, String :: Int :: HNil] = at(str => str :: str.length :: HNil)
  implicit val booleanCase: Case.Aux[Boolean, Boolean :: Int :: HNil] = at(bool => bool :: (if (bool) 1 else 0) :: HNil)
}

object sum extends Poly2 {
  implicit val intIntCase: Case.Aux[Int, Int, Int] = at((x, y) => x + y)
  implicit val intStringCase: Case.Aux[Int, String, Int] = at((i, s) => i + s.length)
}

object conversions extends Poly1 {
  implicit val intCase: Case.Aux[Int, Boolean] = at(_ > 0)
  implicit val boolCase: Case.Aux[Boolean, Int] = at(if (_) 1 else 0)
  implicit val strCase: Case.Aux[String, String] = at(identity)
}

trait ProductMapper[A, B, P] {
  def apply(a: A): B
}

object ProductMapper {
  implicit def genericProductMapper[A, B, ARepr <: HList, BRepr <: HList, P <: Poly](implicit
                                                                                     aGen: Generic.Aux[A, ARepr],
                                                                                     bGen: Generic.Aux[B, BRepr],
                                                                                     map: Mapper.Aux[P, ARepr, BRepr]
                                                                                    ): ProductMapper[A, B, P] = new ProductMapper[A, B, P] {
    override def apply(a: A): B = bGen.from(map.apply(aGen.to(a)))
  }

  implicit class ProductMapperOps[A](a: A) {

    class Builder[B] {
      def apply[P <: Poly](poly: P)(implicit pm: ProductMapper[A, B, P]): B =
        pm.apply(a)
    }

    def mapTo[B]: Builder[B] = new Builder[B]
  }

}

object FunctionalOperations extends App {
  // Polymorphic functions (suitable for mapping over heterogeneous data structures)
  val res1: Double = myPoly[Int](123)
  val res2: Int = myPoly[String]("hello")

  val res3: Int = multiply(3, 4)
  val res4: String = multiply(3, "4")

  total(10)
  total(Option(20.0))
  total(List(1L, 2L, 3L))
  total(List.empty[Int])

  (10 :: "hello" :: true :: HNil).map(sizeOf)
  (10 :: "hello" :: true :: HNil).flatMap(valueAndSizeOf)
  (10 :: "hello" :: 100 :: HNil).foldLeft(0)(sum)

  case class IceCream1(name: String, numCherries: Int, inCone: Boolean)

  case class IceCream2(name: String, hasCherries: Boolean, numCones: Int)

  import ProductMapper._

  IceCream1("Sundae", 1, false).mapTo[IceCream2](conversions)
}
