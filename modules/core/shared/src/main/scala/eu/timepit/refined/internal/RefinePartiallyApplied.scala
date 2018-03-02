package eu.timepit.refined.internal

import eu.timepit.refined.api.{RefType, Validate}
import eu.timepit.refined.boolean.Not

/**
 * Helper class that allows the type `T` to be inferred from calls like
 * `[[api.RefType.refine]][P](t)`.
 *
 * See [[http://tpolecat.github.io/2015/07/30/infer.html]] for a detailed
 * explanation of this trick.
 */
final class RefinePartiallyApplied[F[_, _], P](rt: RefType[F]) {

  def apply[T](t: T)(implicit v: Validate[T, P]): Either[String, F[T, P]] = {
    val res = v.validate(t)
    if (res.isPassed) Right(rt.unsafeWrap(t))
    else Left(v.showResult(t, res))
  }

  def orNot[T](t: T)(implicit v: Validate[T, P]): Either[F[T, Not[P]], F[T, P]] =
    v.validate(t)
      .fold(
        _ => Right(rt.unsafeWrap(t)),
        _ => Left(rt.unsafeWrap(t))
      )

  def unsafeFrom[T](t: T)(implicit v: Validate[T, P]): F[T, P] =
    apply(t).fold(err => throw new IllegalArgumentException(err), identity)
}
