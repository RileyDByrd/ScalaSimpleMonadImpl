package byrd.riley.scalasimplemonadimpl

trait Semigroupal[F[_]]:
  def product[A, B](wrapper1: F[A], wrapper2: F[B]): F[TupleHelper.FlatConcat[A, B]]

  // This law does not apply to Scala 2 because it doesn't natively support bijection for tuples.
  def associativeLaw[A, B, C](wrapper1: F[A], wrapper2: F[B], wrapper3: F[C]): Unit =
    assert(product(wrapper1, product(wrapper2, wrapper3)) == product(product(wrapper1, wrapper2), wrapper3))


object Semigroupal:
  def apply[F[A]](using semigroupal: Semigroupal[F]): Semigroupal[F] = semigroupal

  extension[F[_], A](wrapper1: F[A])(using semigroupal: Semigroupal[F])
    def product[B](wrapper2: F[B]): F[TupleHelper.FlatConcat[A, B]] = semigroupal.product(wrapper1, wrapper2)
