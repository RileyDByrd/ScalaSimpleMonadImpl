package byrd.riley.scalasimplemonadimpl

trait Apply[F[_]] extends Semigroupal[F] with Functor[F] {
  // We don't use "apply" because it means something else in Scala, so we use "ap".
  def ap[A, B](application: F[A => B])(wrapper: F[A]): F[B]

  // Implement the 2 and 3 versions of methods so that implementors of this trait only have to worry about the 1 version.
  // Any one of product, ap, and map may be defined in terms of the other two.
  def ap2[A, B, C](application: F[(A, B) => C])(wrapper1: F[A], wrapper2: F[B]): F[C] =
    map(product(wrapper1, product(wrapper2, application))) { case (value1, (value2, func)) => func(value1, value2) }
  def ap3[A, B, C, D](application: F[(A, B, C) => D])(wrapper1: F[A], wrapper2: F[B], wrapper3: F[C]): F[D] =
    map(product(wrapper1, product(wrapper2, product(wrapper3, application)))) {
      case (value1, (value2, (value3, func))) => func(value1, value2, value3)
    }
}

object Apply {
  def apply[F[A]](implicit apply: Apply[F]): Apply[F] = apply

  implicit class ApplyOps[F[_], A, B](application: F[A => B])(implicit apply: Apply[F]) {
    def ap(wrapper: F[A]): F[B] = apply.ap(application)(wrapper)
  }

  implicit class ApplyTuple2Ops[F[_], A, B](tuple2: (F[A], F[B]))(implicit apply: Apply[F]) {
    def apWith[C](application: F[(A, B) => C]): F[C] = apply.ap2(application)(tuple2._1, tuple2._2)
  }
  implicit class ApplyTuple3Ops[F[_], A, B, C](tuple3: (F[A], F[B], F[C]))(implicit apply: Apply[F]) {
    def apWith[D](application: F[(A, B, C) => D]): F[D] = apply.ap3(application)(tuple3._1, tuple3._2, tuple3._3)
  }
}
