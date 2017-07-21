package guide.chapter5

import shapeless.labelled.{FieldType, field}
import shapeless.syntax.singleton._
import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil, Inl, Inr, LabelledGeneric, Lazy, Witness}

// Deriving product instances with LabelledGeneric
sealed trait JsonValue

final case class JsonObject(fields: List[(String, JsonValue)]) extends JsonValue

final case class JsonArray(items: List[JsonValue]) extends JsonValue

final case class JsonString(value: String) extends JsonValue

final case class JsonNumber(value: Double) extends JsonValue

final case class JsonBoolean(value: Boolean) extends JsonValue

final case object JsonNull extends JsonValue

trait JsonEncoder[A] {
  def encode(value: A): JsonValue
}

object JsonEncoder {
  def apply[A](implicit enc: JsonEncoder[A]): JsonEncoder[A] = enc

  def createEncoder[A](func: A => JsonValue): JsonEncoder[A] = new JsonEncoder[A] {
    override def encode(value: A) = func(value)
  }

  implicit val stringEncoder: JsonEncoder[String] = createEncoder(value => JsonString(value.toString))
  implicit val doubleEncoder: JsonEncoder[Double] = createEncoder(value => JsonNumber(value))
  implicit val intEncoder: JsonEncoder[Int] = createEncoder(value => JsonNumber(value))
  implicit val booleanEncoder: JsonEncoder[Boolean] = createEncoder(value => JsonBoolean(value))

  implicit def listEncoder[A](implicit enc: JsonEncoder[A]): JsonEncoder[List[A]] = createEncoder(list => JsonArray(list.map(enc.encode(_))))

  implicit def optionEncoder[A](implicit enc: JsonEncoder[A]): JsonEncoder[Option[A]] = createEncoder(opt => opt.fold[JsonValue](JsonNull)(enc.encode(_)))

  def createObjectEncoder[A](fn: A => JsonObject): JsonObjectEncoder[A] = new JsonObjectEncoder[A] {
    def encode(value: A): JsonObject = fn(value)
  }

  // Handle products
  implicit val hnilObjectEncoder: JsonObjectEncoder[HNil] = createObjectEncoder(hnil => JsonObject(List.empty))

  implicit def hlistObjectEncoder[K <: Symbol, H, T <: HList](implicit
                                                              witness: Witness.Aux[K],
                                                              hEncoder: Lazy[JsonEncoder[H]],
                                                              tEncoder: JsonObjectEncoder[T]
                                                             ): JsonObjectEncoder[FieldType[K, H] :: T] = {
    val fieldName = witness.value.name

    createObjectEncoder { hlist =>
      val head = hEncoder.value.encode(hlist.head)
      val tail = tEncoder.encode(hlist.tail)

      JsonObject((fieldName, head) :: tail.fields)
    }
  }

  // Handle co-products
  implicit val cnilObjectEncoder: JsonObjectEncoder[CNil] = createObjectEncoder(cnil => throw new Exception("Inconceivable!"))

  implicit def coproductObjectEncoder[K <: Symbol, H, T <: Coproduct](implicit
                                                                      witness: Witness.Aux[K],
                                                                      hEncoder: Lazy[JsonEncoder[H]],
                                                                      tEncoder: JsonObjectEncoder[T]
                                                                     ): JsonObjectEncoder[FieldType[K, H] :+: T] = createObjectEncoder { coproduct =>
    val fieldName = witness.value.name

    coproduct match {
      case Inl(value) => JsonObject(List(fieldName -> hEncoder.value.encode(value)))
      case Inr(value) => tEncoder.encode(value)
    }
  }

  implicit def genericObjectEncoder[A, R](implicit
                                          gen: LabelledGeneric.Aux[A, R],
                                          enc: Lazy[JsonObjectEncoder[R]]
                                         ): JsonEncoder[A] = createObjectEncoder(value => enc.value.encode(gen.to(value)))
}

trait JsonObjectEncoder[A] extends JsonEncoder[A] {
  def encode(value: A): JsonObject
}

final case class IceCream(name: String, numCherries: Int, inCone: Boolean)

sealed trait Shape

final case class Rectangle(width: Double, height: Double) extends Shape

final case class Circle(radius: Double) extends Shape

object AccessingNames extends App {

  // Singleton types
  object Foo

  val singletonType: Foo.type = Foo // That's a singleton type - it belongs exclusively to that one value

  // Literal types
  val x = 42.narrow // x is of type Int(42) now
  val f = "fieldName".narrow

  // Phantom types - types with no runtime semantics
  trait Cherries

  val number = 42
  val numCherries = number.asInstanceOf[Int with Cherries]

  // Type tagging (using nice shapeless syntax)
  val someNumber = 123

  /*
   * Type: Int with shapeless.labelled.KeyTag[String("numCherries"), Int]
   *
   * -> shapeless.labelled.KeyTag[String("numCherries"), Int] (is a phantom type)
   * -> String("numCherries") (is a literal type)
   */
  val numCherries2 = "numCherries" ->> someNumber

  // shapeless.labelled.FieldType[Cherries,Int] => type FieldType[K, V] = V with KeyTag[K, V]
  val numCherries3 = field[Cherries](123)

  // Get the tag from a tagged value:
  def getFieldName[K, V](value: FieldType[K, V])(implicit witness: Witness.Aux[K]): K = witness.value

  def getFieldValue[K, V](value: FieldType[K, V]): V = value

  val name = getFieldName(numCherries2) // "numCherries": String
  val num: Int = getFieldValue(numCherries2) // 123: Int

  // An HList of labeled types if referred to as "record"
  val garfield = ("cat" ->> "Garfield") :: ("orange" ->> true) :: HNil

  // Generic instances for products
  val iceCream = IceCream("Sundae", 1, false)
  val genIceCream = LabelledGeneric[IceCream].to(iceCream)

  val json = JsonEncoder[IceCream].encode(iceCream)
  println(json)

  // Generic instances for co-products
  val circle = Circle(1.0)
  val genCircle = LabelledGeneric[Shape].to(circle)

  JsonEncoder[Shape].encode(circle)
  val circleJson = JsonEncoder[Shape].encode(circle)
  println(circleJson)
}
