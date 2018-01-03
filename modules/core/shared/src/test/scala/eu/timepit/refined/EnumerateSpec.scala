package eu.timepit.refined

import eu.timepit.refined.api.{Enumerate, Refined, Validate}
import eu.timepit.refined.numeric._
import org.scalacheck.Prop._
import org.scalacheck.Properties
import shapeless.nat._

class EnumerateSpec extends Properties("Min") {

  private def checkAllValues[T, P](expectedHead: T, expectedLast: T, expectedSize: Int)(
      implicit enumerate: Enumerate[T Refined P],
      validate: Validate[T, P],
      numeric: Numeric[T]) = {
    val result = enumerate.enumerate
    result.head.value =? expectedHead &&
    result.last.value =? expectedLast &&
    result.size =? expectedSize &&
    result.sliding(2).exists {
      case Stream(previous, next) => numeric.gteq(previous.value, next.value)
    } =? false &&
    result.exists(r => !Validate[T, P].isValid(r.value)) =? false
  }

  property("Enumerate[Byte Refined Greater[_1]]") = secure {
    checkAllValues[Byte, Greater[_1]](2, Byte.MaxValue, Byte.MaxValue - 1)
  }

  property("Enumerate[Float Refined Interval.Closed[W.`1.0f`.T, W.`1.1f`.T]]") = secure {
    checkAllValues[Float, Interval.Closed[W.`1.0f`.T, W.`1.1f`.T]](1.0f, 1.1f, 838862)
  }
}
