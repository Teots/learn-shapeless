package guide.chapter8

import shapeless.ops.nat.ToInt
import shapeless.ops.{coproduct, hlist}
import shapeless.{Nat, Succ, _}

trait SizeOf[A] {
  def value: Int
}

object SizeOf {
  def sizeOf[A](implicit size: SizeOf[A]): Int = size.value

  implicit def genericSizeOf[A, Repr <: HList, N <: Nat](implicit
                                                         gen: Generic.Aux[A, Repr],
                                                         len: hlist.Length.Aux[Repr, N],
                                                         sizeToInt: ToInt[N]
                                                        ): SizeOf[A] = new SizeOf[A] {
    override def value: Int = sizeToInt.apply()
  }
}

trait Random[A] {
  def get: A
}

object Random {
  def random[A](implicit rand: Random[A]): A = rand.get

  // Instance constructor
  def createRandom[A](func: () => A): Random[A] = new Random[A] {
    override def get: A = func()
  }

  // Random numbers from 0 to 9
  implicit val intRandom = createRandom(() => scala.util.Random.nextInt(10))

  // Random characters from A to Z
  implicit val charRandom: Random[Char] = createRandom(() => ('A'.toInt + scala.util.Random.nextInt(26)).toChar)

  // Random booleans
  implicit val booleanRandom: Random[Boolean] = createRandom(() => scala.util.Random.nextBoolean)

  implicit def genericRandom[A, Repr](implicit
                                      gen: Generic.Aux[A, Repr],
                                      random: Lazy[Random[Repr]]
                                     ): Random[A] = createRandom(() => gen.from(random.value.get))

  // Random products
  implicit val hnilRandom: Random[HNil] = createRandom(() => HNil)

  implicit def hlistRandom[H, T <: HList](implicit
                                          hRand: Lazy[Random[H]],
                                          tRand: Random[T]
                                         ): Random[H :: T] = new Random[H :: T] {
    override def get: H :: T = hRand.value.get :: tRand.get
  }

  // Random coproducts
  implicit val cnilRandom: Random[CNil] = createRandom(() => throw new Exception("Inconceivable!"))

  implicit def clistRandom[H, T <: Coproduct, N <: Nat](implicit
                                                        hRand: Lazy[Random[H]],
                                                        tRand: Random[T],
                                                        tLen: coproduct.Length.Aux[T, N],
                                                        remainingLength: ToInt[N]
                                                       ): Random[H :+: T] = createRandom { () =>
    // Choose H 1/n of the time
    val length = remainingLength.apply() + 1
    val chooseH = scala.util.Random.nextDouble < (1.0d / length)
    if (chooseH) Inl(hRand.value.get) else Inr(tRand.get)
  }
}

object CountingWithTypes extends App {
  type Zero = Nat._0
  type One = Succ[Zero]
  type Two = Succ[One]

  val toInt = ToInt[Two]
  Nat.toInt[Nat._3]

  // Extraction of elements
  val exampleList = 123 :: "foo" :: true :: 'x' :: HNil
  val name: String = exampleList.apply[Nat._1]
  val charName: Char = exampleList.apply[Nat._3]

  // More number based operations on HList
  exampleList.take(Nat._3).drop(Nat._1)
  exampleList.updatedAt(Nat._1, "bar").updatedAt(Nat._2, "baz")

  // Length of products and coproducts
  val hlistLength = hlist.Length[String :: Int :: Boolean :: HNil]
  Nat.toInt[hlistLength.Out]
  val coproductLength = coproduct.Length[Double :+: Char :+: CNil]
  Nat.toInt[coproductLength.Out]

  case class IceCream(name: String, numCherries: Int, inCone: Boolean)

  import SizeOf._

  sizeOf[IceCream]

  // Random products
  import Random._

  case class Cell(col: Char, row: Int)

  for (i <- 1 to 5) println(random[Cell])

  // Random coproducts
  sealed trait Light

  case object Red extends Light

  case object Amber extends Light

  case object Green extends Light

  for (i <- 1 to 10) println(random[Light])
}
