package feature_overview

import shapeless._

object BoilerplateFreeLensesForArbitraryCaseClasses extends App {

  // A pair of ordinary case classes ...
  case class Address(street: String, city: String, postcode: String)

  case class Person(name: String, age: Int, address: Address)

  // Some lenses over Person/Address ...
  val nameLens = lens[Person] >> 'name
  val ageLens = lens[Person] >> 'age
  val addressLens = lens[Person] >> 'address
  val streetLens = lens[Person] >> 'address >> 'street
  val cityLens = lens[Person] >> 'address >> 'city
  val postcodeLens = lens[Person] >> 'address >> 'postcode

  val person = Person("Joe Grey", 37, Address("Southover Street", "Brighton", "BN2 9UA"))
  val age1: Int = ageLens.get(person)

  val person2 = ageLens.set(person)(38)
  val person3 = ageLens.modify(person2)(_ + 1)
  val person4 = streetLens.set(person3)("Montpelier Road")

  val street: String = streetLens.get(person4)
}
