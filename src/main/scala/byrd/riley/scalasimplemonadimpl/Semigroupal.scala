package byrd.riley.scalasimplemonadimpl

trait Semigroupal[F[_]] {
  def product[A, B](wrapper1: F[A], wrapper2: F[B]): F[(A, B)]

  // This law does not apply to Scala 2 because it doesn't natively support bijection for tuples.
  //  def associativeLaw[A, B, C](wrapper1: F[A], wrapper2: F[B], wrapper3: F[C]): Boolean = {
  //    assert(product(wrapper1, product(wrapper2, wrapper3)) == product(product(wrapper1, wrapper2), wrapper3))
  //  }
}

object Semigroupal {
  def apply[F[A]](implicit semigroupal: Semigroupal[F]): Semigroupal[F] = semigroupal

  implicit class SemigroupalOps[F[_], A](wrapper1: F[A])(implicit semigroupal: Semigroupal[F]) {
    def product[B](wrapper2: F[B]): F[(A, B)] = semigroupal.product(wrapper1, wrapper2)
  }
}