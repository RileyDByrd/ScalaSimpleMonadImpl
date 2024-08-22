package byrd.riley.scalasimplemonadimpl

// "Applicative" is short for "Applicative Functor".
trait Applicative[F[_]] extends Apply[F]:
  // i.e. wrap
  // Does not override .pure if a class has already implemented it, which means Monad instead of MyMonad will be used.
  extension[A](value: A)
    def pure: F[A]

  // Provide a cleaner implementation of the identityApplyLaw since pure is available.
  override def identityApplyLaw[A](applicative: F[A]): Unit =
    assert(identity[A].pure.ap(applicative) == applicative)
