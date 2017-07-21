package guide.chapter3

import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, Lazy}

// Type class - Turn a value of type A into a row of cells in a CSV file
trait CsvEncoder[A] {
  def encode(value: A): List[String]
}

object CsvEncoder {
  // "Summoner" or "Materializer" method
  def apply[A](implicit env: CsvEncoder[A]): CsvEncoder[A] = env

  // "Constructor" method (sometimes named "pure")
  def instance[A](func: A => List[String]): CsvEncoder[A] = new CsvEncoder[A] {
    override def encode(value: A): List[String] = func(value)
  }

  // Derive type classes for product types
  implicit val stringEncoder: CsvEncoder[String] = instance(str => List(str))
  implicit val intEncoder: CsvEncoder[Int] = instance(num => List(num.toString))
  implicit val doubleEncoder: CsvEncoder[Double] = instance(num => List(num.toString))
  implicit val booleanEncoder: CsvEncoder[Boolean] = instance(bool => List(if (bool) "yes" else "no"))

  // Combine those encoders to an encoder for HList
  implicit val hnilEncoder: CsvEncoder[HNil] = instance(hnil => List.empty)

  implicit def hlistEncoder[H, T <: HList](implicit hEncoder: Lazy[CsvEncoder[H]], tEncoder: CsvEncoder[T]): CsvEncoder[H :: T] = instance { case head :: tail =>
    hEncoder.value.encode(head) ++ tEncoder.encode(tail)
  }

  // Generic encoder: Given a type A and an HList type R, an implicit Generic to map A to R, and a CsvEncoder for R, create a CsvEncoder for A.
  //  implicit def genericEncoder[A, R](implicit
  //                                 gen: Generic[A] { type Repr = R },
  //                                 enc: CsvEncoder[R]
  //                                ) = instance[A](elem => enc.encode(gen.to(elem)))

  // Same as above, just more terse syntax
  implicit def gernericEncoder[A, R](implicit
                                     gen: Generic.Aux[A, R],
                                     enc: Lazy[CsvEncoder[R]]
                                    ) = instance[A](elem => enc.value.encode(gen.to(elem)))

  //   Encoder for co-products
  implicit val cnilEncoder: CsvEncoder[CNil] = instance(cnil => throw new Exception("Inconceivable!"))

  implicit def clistEncoder[H, T <: Coproduct](implicit
                                               hEncoder: Lazy[CsvEncoder[H]],
                                               tEncoder: CsvEncoder[T]
                                              ): CsvEncoder[H :+: T] = instance {
    _ match {
      case Inl(h) => hEncoder.value.encode(h)
      case Inr(t) => tEncoder.encode(t)
    }
  }
}

// Custom data type
final case class Employee(name: String, number: Int, manager: Boolean)

// CsvEncoder instance for the custom data type
object Employee {
  implicit val employeeEncoder: CsvEncoder[Employee] = new CsvEncoder[Employee] {
    override def encode(e: Employee): List[String] =
      List(
        e.name,
        e.number.toString,
        if (e.manager) "yes" else "no"
      )
  }
}

final case class IceCream(name: String, numCherries: Int, inCone: Boolean)

object IceCream {

  import CsvEncoder._

  // Version 1
  //  implicit val iceCreamEncoder: CsvEncoder[IceCream] = CsvEncoder.instance(i => // Terse syntax to reduce boilerplate of anonymous classes
  //    List(
  //      i.name,
  //      i.numCherries.toString,
  //      if (i.inCone) "yes" else "no"
  //    )
  //  )

  // Version 2 - using the facts that we have a product (icecream), a generic representation of it and know how to encode that
  implicit val iceCreamEncoder: CsvEncoder[IceCream] = {
    val gen = Generic[IceCream]
    val enc = CsvEncoder[gen.Repr]
    CsvEncoder.instance(iceCream => enc.encode(gen.to(iceCream)))
  }
}

// Co-product
sealed trait Shape

final case class Rectangle(width: Double, height: Double) extends Shape

final case class Circle(radius: Double) extends Shape

// diverging implicit expansion - Implicit resolution will fail for those classes (if we wouldn't use lazy)
final case class Bar(baz: Int, qux: String)

final case class Foo(bar: Bar)

sealed trait Tree[A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

object DeriveTypeClasses extends App {
  def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String =
    values.map(value => enc.encode(value).mkString(",")).mkString("\n")

  val employees: List[Employee] = List(
    Employee("Bill", 1, true),
    Employee("Peter", 2, false),
    Employee("Milton", 3, false)
  )

  println(writeCsv(employees))

  val iceCreams: List[IceCream] = List(
    IceCream("Sundae", 1, false),
    IceCream("Cornetto", 0, true),
    IceCream("Banana Split", 0, false)
  )

  println(writeCsv(iceCreams))

  val shapes: List[Shape] = List(
    Rectangle(3.0, 4.0),
    Circle(1.0)
  )

  println(writeCsv(shapes))

  // Those examples would fail without using lazy
  val foo = Foo(Bar(1, "a"))
  println(writeCsv(List(foo)))

  val tree = Branch[String](Branch[String](Leaf("a"), Leaf("b")), Leaf("c"))
  println(writeCsv(List(tree)))
}
