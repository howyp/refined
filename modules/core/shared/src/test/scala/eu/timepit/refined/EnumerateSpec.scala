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
    ("First value" |: expectedHead =? result.head.value) &&
    ("Last value" |: expectedLast =? result.last.value) &&
    ("Size" |: expectedSize =? result.size) &&
    ("Values are ascending" |: false =? result.sliding(2).exists {
      case Stream(previous, next) => numeric.gteq(previous.value, next.value)
    }) &&
    ("Values are all valid" |: false =? result.exists(r => !Validate[T, P].isValid(r.value)))
  }

  property("Enumerate[Byte Refined Greater[_1]]") = secure {
    checkAllValues[Byte, Greater[_1]](expectedHead = 2,
                                      expectedLast = Byte.MaxValue,
                                      expectedSize = Byte.MaxValue - 1)
  }

  property("Enumerate[Float Refined Interval.Closed[W.`1.0f`.T, W.`1.1f`.T]]") = secure {
    checkAllValues[Float, Interval.Closed[W.`1.0f`.T, W.`1.1f`.T]](expectedHead = 1.0f,
                                                                   expectedLast = 1.1f,
                                                                   expectedSize = 838862)
  }

  property("Enumerate[Byte Refined Odd]") = secure {
    checkAllValues[Byte, Odd](expectedHead = -127, expectedLast = 127, expectedSize = 128)
  }
}
