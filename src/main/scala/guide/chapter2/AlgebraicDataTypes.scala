package guide.chapter2

import shapeless.{::, Generic, HList, HNil}

// That's a co-product (aka. an or-type) - it's either a Rectangle OR a Circle
sealed trait Shape

// That's a product (aka. an and-type) - it has a width AND a height
final case class Rectangle(width: Double, height: Double) extends Shape

final case class Circle(radius: Double) extends Shape

/*
 * Alternative encoding:
 */
object AlternativeEncoding {
  type Rectangle2 = (Double, Double)
  type Circle2 = Double
  type Shape2 = Either[Rectangle2, Circle2]
}

object AlgebraicDataTypes extends App {

  // Version 1
  def area(shape: Shape): Double =
    shape match {
      case Rectangle(w, h) => w * h
      case Circle(r) => math.Pi * r * r
    }

  val rect: Shape = Rectangle(3.0, 4.0)
  val circ: Shape = Circle(1.0)

  area(rect)
  area(circ)

  // Version 2
  import AlternativeEncoding._

  def area2(shape: Shape2): Double =
    shape match {
      case Left((w, h)) => w * h
      case Right(r)     => math.Pi * r * r
    }

  val rect2: Shape2 = Left((3.0, 4.0))
  val circ2: Shape2 = Right(1.0)

  area2(rect2)
  area2(circ2)

  // Version 3 - Generic
  val coProductGen = Generic[Shape]

  coProductGen.to(Rectangle(3.0, 4.0))
  coProductGen.to(Circle(1.0))

  val productGen = Generic[Rectangle]
  val repr = productGen.to(Rectangle(3.0, 4.0))
  val rect3 = productGen.from(repr)
}
