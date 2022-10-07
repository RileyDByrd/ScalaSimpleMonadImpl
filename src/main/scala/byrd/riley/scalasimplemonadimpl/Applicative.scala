package byrd.riley.scalasimplemonadimpl

// "Applicative" is short for "Applicative Functor".
trait Applicative[F[_]] extends Apply[F] {
  // i.e. wrap
  def pure[A](value: A): F[A]

  // A `pure` of the value inside a wrapper is equal to the wrapper instance.
  def identityLaw[A](wrapper: F[A]): Unit =
    assert(ap(pure((value: A) => value))(wrapper) == wrapper)

  def map2[A, B, C](wrapper1: F[A], wrapper2: F[B])(func: (A, B) => C): F[C] =
    ap2(pure(func))(wrapper1, wrapper2)
  def map3[A, B, C, D](wrapper1: F[A], wrapper2: F[B], wrapper3: F[C])(func: (A, B, C) => D): F[D] =
    ap3(pure(func))(wrapper1, wrapper2, wrapper3)

  def tuple2[A, B](wrapper1: F[A], wrapper2: F[B]): F[(A, B)] =
    product(wrapper1, wrapper2)
  def tuple3[A, B, C](wrapper1: F[A], wrapper2: F[B], wrapper3: F[C]): F[(A, B, C)] =
    map(product(product(wrapper1, wrapper2), wrapper3)) { case ((value1, value2), value3) => (value1, value2, value3) }
}

object Applicative {
  import Apply._

  def apply[F[_]](implicit applicative: Applicative[F]): Applicative[F] = applicative

  // Does not override .pure if a class has already implemented it, which means Monad instead of MyMonad will be used.
  implicit class ApplicativeOps[F[_], A](value: A)(implicit applicative: Applicative[F]) {
    def pure: F[A] = applicative.pure(value)
  }


  implicit class ApplicativeTuple2Ops[F[_], A, B](tuple2: (F[A], F[B]))(implicit applicative: Applicative[F]) {
    def mapN[C](func: (A, B) => C): F[C] = tuple2.apWith(func.pure)
    def tupled: F[(A, B)] = tuple2.mapN((_, _))
  }
  implicit class ApplicativeTuple3Ops[F[_], A, B, C](tuple3: (F[A], F[B], F[C]))(implicit applicative: Applicative[F]) {
    def mapN[D](func: (A, B, C) => D): F[D] = tuple3.apWith(func.pure)
    def tupled: F[(A, B, C)] = tuple3.mapN((_, _, _))
  }
}