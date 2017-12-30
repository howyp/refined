package eu.timepit.refined

import eu.timepit.refined.api.{Enumerate, Refined}
//import eu.timepit.refined.boolean._
import eu.timepit.refined.numeric._
//import eu.timepit.refined.types.numeric._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import shapeless.nat._

class EnumerateSpec extends Properties("Min") {

  property("Enumerate[Byte Refined Greater[_1]]") = secure {
    val v = Enumerate[Byte Refined Greater[_1]].enumerate
    v.head =? refineMV(2) &&
    v.last =? refineMV(Byte.MaxValue) &&
    v.size =? (Byte.MaxValue - 1)
  }
}
