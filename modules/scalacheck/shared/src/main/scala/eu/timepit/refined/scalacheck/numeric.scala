package eu.timepit.refined
package scalacheck

import eu.timepit.refined.api._
import org.scalacheck.Gen.Choose
import org.scalacheck.{Arbitrary, Gen}

/**
 * Module that provides `Arbitrary` instances and generators for
 * numeric predicates.
 */
object numeric extends NumericInstances

trait NumericInstances {

  /**
   * A generator that generates a random value in the given (inclusive)
   * range that satisfies the predicate `P`. If the range is invalid,
   * the generator will not generate any value.
   *
   * This is like ScalaCheck's `Gen.chooseNum` but for refined types.
   */
  def chooseRefinedNum[F[_, _], T: Numeric: Choose, P](min: F[T, P], max: F[T, P])(
      implicit rt: RefType[F],
      v: Validate[T, P]
  ): Gen[F[T, P]] =
    Gen.chooseNum(rt.unwrap(min), rt.unwrap(max)).filter(v.isValid).map(rt.unsafeWrap)

  ///

  implicit def arbitraryFromMaxAndMin[C, N](implicit min: Min[C Refined N],
                                            max: Max[C Refined N],
                                            choose: Choose[C]): Arbitrary[C Refined N] =
    Arbitrary(Gen.choose(min.min.value, max.max.value).map(Refined.unsafeApply[C, N]))
}
