package feature_overview

import shapeless.HMap

object HeterogenousMaps extends App {

  // Key/value relation to be enforced: Strings map to Ints and vice versa
  class BiMapIS[K, V]

  implicit val intToString = new BiMapIS[Int, String]
  implicit val stringToInt = new BiMapIS[String, Int]

  val hm = HMap[BiMapIS](23 -> "foo", "bar" -> 13)
  //val hm2 = HMap[BiMapIS](23 -> "foo", 23 -> 13) // Does not compile

  println(hm.get(23))
  println(hm.get("bar"))
}
