package byrd.riley.scalasimplemonadimpl

trait Semigroupal[F[_]] {
  def product[A, B](wrapper1: F[A], wrapper2: F[B]): F[(A, B)]

  // This law does not apply to Scala 2 because it doesn't natively support bijection for tuples.
  //  def associativeLaw[A, B, C](wrapper1: F[A], wrapper2: F[B], wrapper3: F[C]): Boolean = {
  //    assert(product(wrapper1, product(wrapper2, wrapper3)) == product(product(wrapper1, wrapper2), wrapper3))
  //  }

  def tuple2[A, B](wrapper1: F[A], wrapper2: F[B]): F[(A, B)]
  def tuple3[A, B, C](wrapper1: F[A], wrapper2: F[B], wrapper3: F[C]): F[(A, B, C)]
}

object Semigroupal {
  def apply[F[A]](implicit semigroupal: Semigroupal[F]): Semigroupal[F] = semigroupal

  implicit class SemigroupalOps[F[_], A](wrapper1: F[A])(implicit semigroupal: Semigroupal[F]) {
    def product[B](wrapper2: F[B]): F[(A, B)] = semigroupal.product(wrapper1, wrapper2)
  }

  implicit class Tuple2Ops[F[_], A, B](tuple2: (F[A], F[B]))(implicit semigroupal: Semigroupal[F]) {
    def tupled: F[(A, B)] = semigroupal.tuple2(tuple2._1, tuple2._2)
  }
  implicit class Tuple3Ops[F[_], A, B, C](tuple3: (F[A], F[B], F[C]))(implicit semigroupal: Semigroupal[F]) {
    def tupled: F[(A, B, C)] = semigroupal.tuple3(tuple3._1, tuple3._2, tuple3._3)
  }
}