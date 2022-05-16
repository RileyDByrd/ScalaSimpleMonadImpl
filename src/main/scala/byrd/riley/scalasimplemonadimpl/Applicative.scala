package byrd.riley.scalasimplemonadimpl

// "Applicative" is short for "Applicative Functor".
trait Applicative[F[_]] extends Apply[F] {
  // i.e. wrap
  def pure[A](value: A): F[A]

  // A `pure` of the value inside a wrapper is equal to the wrapper instance.
  def identityLaw[A](wrapper: F[A]): Unit =
    assert(ap(pure((value: A) => value))(wrapper) == wrapper)
}

object Applicative {
  def apply[F[_]](implicit applicative: Applicative[F]): Applicative[F] = applicative

  // Does not override .pure if a class has already implemented it, which means Monad instead of MyMonad will be used.
  implicit class ApplicativeOps[F[_], A](value: A)(implicit applicative: Applicative[F]) {
    def pure: F[A] = applicative.pure(value)
  }
}