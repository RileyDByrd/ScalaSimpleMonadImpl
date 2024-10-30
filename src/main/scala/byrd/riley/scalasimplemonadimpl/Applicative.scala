package byrd.riley.scalasimplemonadimpl

// "Applicative" is short for "Applicative Functor".
trait Applicative extends Apply:
  // i.e. wrap
  extension[A](value: A)
    def pure: Self[A]

  // Provide a cleaner implementation of the identityApplyLaw since pure is available.
//  override def identityApplyLaw[A](applicative: Self[A]): Unit =
//    assert(identity[A].pure.ap(applicative) == applicative)
