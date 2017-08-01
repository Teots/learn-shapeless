package guide.chapter6

import cats.Monoid
import cats.instances.all._
import shapeless._
import shapeless.labelled.{FieldType, field}
import shapeless.ops.hlist.{Align, Diff, Init, Intersection, Last, Prepend}

trait Penultimate[L] {
  type Out

  def apply(l: L): Out
}

object Penultimate {
  type Aux[L, O] = Penultimate[L] {type Out = O}

  def apply[L](implicit p: Penultimate[L]): Aux[L, p.Out] = p

  implicit def hlistPenultimate[L <: HList, O, initOut <: HList](implicit
                                                                 init: Init.Aux[L, initOut],
                                                                 last: Last.Aux[initOut, O],
                                                                ): Penultimate.Aux[L, O] = new Penultimate[L] {
    type Out = O

    override def apply(l: L): Out = last(init(l))
  }

  // Generic implementaion
  implicit def genericPenultimate[A, R, O](implicit
                                           gen: Generic.Aux[A, R],
                                           p: Penultimate.Aux[R, O]
                                          ): Penultimate.Aux[A, O] = new Penultimate[A] {
    type Out = O

    override def apply(l: A): O = p(gen.to(l))
  }

  // Provide an extension method
  implicit class PenultimateOps[A](a: A) {
    def penultimate(implicit inst: Penultimate[A]): inst.Out =
      inst.apply(a)
  }

}

object MonoidExtensions {
  private def createMonoid[A](zero: A)(add: (A, A) => A): Monoid[A] =
    new Monoid[A] {
      def empty = zero

      def combine(x: A, y: A): A = add(x, y)
    }

  implicit val hnilMonoid: Monoid[HNil] = createMonoid[HNil](HNil) { case (_, _) => HNil }

  implicit def listMonoid[K <: Symbol, H, T <: HList](
                                                       implicit
                                                       hMonoid: Lazy[Monoid[H]],
                                                       tMonoid: Monoid[T]
                                                     ): Monoid[FieldType[K, H] :: T] = createMonoid(field[K](hMonoid.value.empty) :: tMonoid.empty) { case (x, y) =>
    field[K](hMonoid.value.combine(x.head, y.head)) :: tMonoid.combine(x.tail, y.tail)
  }
}

// Migrations, aka. Evolutions on case classes
trait Migration[A, B] {
  def apply(a: A): B
}

object Migration {

  // Remove fields
  implicit def genericMigrationRemove[A, B, ARepr <: HList, BRepr <: HList](implicit
                                                                      genA: LabelledGeneric.Aux[A, ARepr],
                                                                      genB: LabelledGeneric.Aux[B, BRepr],
                                                                      intersection: Intersection.Aux[ARepr, BRepr, BRepr]
                                                                     ): Migration[A, B] = new Migration[A, B] {
    override def apply(a: A): B = genB.from(intersection.apply(genA.to(a)))
  }

  // Reorder fields
  implicit def genericMigrationReorder[A, B, ARepr <: HList, BRepr <: HList, Unaligned <: HList](implicit
                                                                                 genA: LabelledGeneric.Aux[A, ARepr],
                                                                                 genB: LabelledGeneric.Aux[B, BRepr],
                                                                                 intersection: Intersection.Aux[ARepr, BRepr, Unaligned],
                                                                                 align: Align[Unaligned, BRepr]
                                                                                ): Migration[A, B] = new Migration[A, B] {
    override def apply(a: A): B = genB.from(align.apply(intersection.apply(genA.to(a))))
  }

  // Insert fields
  implicit def genericMigrationInsert[A, B, ARepr <: HList, BRepr <: HList, Common <: HList, Added <: HList, Unaligned <: HList](implicit
                                                                                                                           genA: LabelledGeneric.Aux[A, ARepr],
                                                                                                                           genB: LabelledGeneric.Aux[B, BRepr],
                                                                                                                           inter: Intersection.Aux[ARepr, BRepr, Common],
                                                                                                                           diff: Diff.Aux[BRepr, Common, Added],
                                                                                                                           monoid: Monoid[Added],
                                                                                                                           prepend: Prepend.Aux[Added, Common, Unaligned],
                                                                                                                           align: Align[Unaligned, BRepr]
                                                                                                                          ): Migration[A, B] = new Migration[A, B] {
    override def apply(a: A): B = {
      genB.from(align.apply(prepend.apply(monoid.empty, inter.apply(genA.to(a)))))
    }
  }

  // Extension method
  implicit class MigrationsOps[A](a: A) {
    def migrateTo[B](implicit migration: Migration[A, B]) = migration(a)
  }

}

object ADTOps extends App {
  // Operations on HList (an HList is implicitly converted to HListOps) and those ops require type classes, e.g. Last[T] and Init[T]
  val last: Boolean = ("Hello" :: 123 :: true :: HNil).last
  val lastElemRemoved: HList = ("Hello" :: 123 :: true :: HNil).init

  // Creating a custom op (the “lemma” pattern)
  import Penultimate._

  type BigList = String :: Int :: Boolean :: Double :: HNil
  val bigList: BigList = "foo" :: 123 :: true :: 456.0 :: HNil

  val penultimate: Boolean = Penultimate[BigList].apply(bigList)
  val penultimate2: Boolean = bigList.penultimate

  case class IceCream(name: String, numCherries: Int, inCone: Boolean)

  IceCream("Sundae", 1, false).penultimate

  // Migrations, aka. Evolutions on case classes
  case class IceCreamV1(name: String, numCherries: Int, inCone: Boolean)

  import Migration.MigrationsOps

  {
    // Remove fields
    import Migration.genericMigrationRemove
    case class IceCreamV2a(name: String, inCone: Boolean)
    IceCreamV1("Sundae", 1, false).migrateTo[IceCreamV2a]
  }

  {
    // Reorder fields
    import Migration.genericMigrationReorder
    case class IceCreamV2b(name: String, inCone: Boolean, numCherries: Int)
    IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2b]
  }

  {
    // Insert fields (provided we can determine a default value)
    import Migration.genericMigrationInsert
    import MonoidExtensions._
    case class IceCreamV2c(name: String, inCone: Boolean, numCherries: Int, numWaffles: Int)
    IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2c]
  }

  // Ops on records
  import shapeless.record._

  val sundae = LabelledGeneric[IceCream].to(IceCream("Sundae", 1, false))
  val name: String = sundae.get('name)
  sundae.updated('numCherries, 3)
  sundae.remove('inCone)
  sundae.updateWith('name)("MASSIVE " + _)

  val sundaeMap: Map[Symbol, Any] = sundae.toMap
}
