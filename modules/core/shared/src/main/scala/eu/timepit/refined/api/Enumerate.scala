package eu.timepit.refined.api

import eu.timepit.refined.internal.Adjacent

trait Enumerate[T] {
  def enumerate: Stream[T]
}
object Enumerate extends EnumerateInstances {
  def apply[T](implicit i: Enumerate[T]): Enumerate[T] = i
  def instance[T](values: => Stream[T]): Enumerate[T] = new Enumerate[T] {
    override def enumerate = values
  }
}
trait EnumerateInstances {
  implicit def enumerateFromMaxAndMin[F[_, _], T, P](implicit rt: RefType[F],
                                                     min: Min[F[T, P]],
                                                     max: Max[F[T, P]],
                                                     adjacent: Adjacent[T]): Enumerate[F[T, P]] =
    Enumerate.instance {
      def stream(head: F[T, P]): Stream[F[T, P]] = head match {
        case last if last == max.max =>
          Stream(last)
        case next =>
          Stream.cons(next, stream(rt.unsafeWrap(adjacent.nextUp(rt.unwrap(next)))))
      }
      stream(min.min)
    }
}
