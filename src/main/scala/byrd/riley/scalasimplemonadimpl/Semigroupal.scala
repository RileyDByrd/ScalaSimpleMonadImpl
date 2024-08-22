package byrd.riley.scalasimplemonadimpl

trait Semigroupal[F[_]]:
  extension[A](semigroupal1: F[A])
    def product[B](semigroupal2: F[B]): F[TupleHelper.FlatConcat[A, B]]

  // This law does not apply to Scala 2 because it doesn't natively support bijection for tuples.
  def associativeSemigroupalLaw[A, B, C](semigroupal1: F[A], semigroupal2: F[B], semigroupal3: F[C]): Unit =
    assert(semigroupal1.product(semigroupal2.product(semigroupal3)) == semigroupal1.product(semigroupal2).product(semigroupal3))


object Semigroupal:
  def apply[F[A]](using semigroupal: Semigroupal[F]): Semigroupal[F] = semigroupal
