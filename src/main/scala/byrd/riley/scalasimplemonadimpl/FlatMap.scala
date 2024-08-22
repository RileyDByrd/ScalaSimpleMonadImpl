package byrd.riley.scalasimplemonadimpl

trait FlatMap[F[_]] extends Apply[F]:
  extension[A](flatMap: F[A])
    def flatMap[B](func: A => F[B]): F[B]
    
  extension[A](doubleFlatMap: F[F[A]])
    def flatten: F[A] = doubleFlatMap.flatMap(identity)

  // Associativity: flatMapping over f and then flatMapping over g is the same as flatMapping over two functions f and g.
  def associativeFlatMapLaw[A, B, C](flatMap: F[A], funcF: A => F[B], funcG: B => F[C]): Unit =
    assert(flatMap.flatMap(funcF).flatMap(funcG) == flatMap.flatMap(value => funcF(value).flatMap(funcG)))
    
  // Flatmapping a value to itself should yield an equal FlatMap.
  def identityFlatMapLaw[A](flatMap: F[A]): Unit =
    def flatMapIdentity[A](value: A) = flatMap.map(_ => value)
    assert(flatMap.flatMap(flatMapIdentity) == flatMap)
