package eu.timepit.refined.internal

import eu.timepit.refined.api.{RefType, Validate}

/**
 * Type class that provides the next greater or next smaller value for
 * a given argument.
 */
trait Adjacent[T] {

  /**
   * Returns the next greater value adjacent to `t` or `t` if there is
   * no greater value.
   */
  def nextUp(t: T): T

  /**
   * Returns the next smaller value adjacent to `t` or `t` if there is
   * no smaller value.
   */
  def nextDown(t: T): T
}

object Adjacent extends LowPriorityAdjacent {
  def apply[T](implicit a: Adjacent[T]): Adjacent[T] = a

  def instance[T](nextUpF: T => T, nextDownF: T => T): Adjacent[T] =
    new Adjacent[T] {
      override def nextUp(t: T): T = nextUpF(t)
      override def nextDown(t: T): T = nextDownF(t)
    }

  implicit val doubleAdjacent: Adjacent[Double] =
    instance(Math.nextUp, Math.nextDown)

  implicit val floatAdjacent: Adjacent[Float] =
    instance(Math.nextUp, Math.nextDown)

  implicit def integralAdjacent[T](implicit it: Integral[T]): Adjacent[T] =
    instance(
      t => it.max(it.plus(t, it.one), t),
      t => it.min(it.minus(t, it.one), t)
    )
}
trait LowPriorityAdjacent {
  implicit def refinedAdjacentByValidation[F[_, _], T, P](implicit rt: RefType[F],
                                                          at: Adjacent[T],
                                                          vtp: Validate[T, P]): Adjacent[F[T, P]] =
    Adjacent.instance(
      nextUpF = { from =>
        var result = rt.unwrap(from)
        do (result = at.nextUp(result)) while (!vtp.isValid(result))
        rt.unsafeWrap(result)
      },
      nextDownF = { from =>
        var result = rt.unwrap(from)
        while (!vtp.isValid(result)) result = at.nextDown(result)
        rt.unsafeWrap(result)
      }
    )
}
