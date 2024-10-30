package byrd.riley.scalasimplemonadimpl

trait FlatMap extends Apply:
  extension[A](flatMap: Self[A])
    def flatMap[B](func: A => Self[B]): Self[B]
    
  extension[A](doubleFlatMap: Self[Self[A]])
    def flatten: Self[A] = doubleFlatMap.flatMap(identity)
  
  // Associativity: flatMapping over f and then flatMapping over g is the same as flatMapping over two functions f and g.
//  def associativeFlatMapLaw[A, B, C](flatMap: Self[A], funcF: A => Self[B], funcG: B => Self[C]): Unit =
//    assert(flatMap.flatMap(funcF).flatMap(funcG) == flatMap.flatMap(value => funcF(value).flatMap(funcG)))
    
  // Flatmapping a value to itself should yield an equal FlatMap.
//  def identityFlatMapLaw[A](flatMap: F[A]): Unit =
//    def flatMapIdentity[A](value: A) = flatMap.map(_ => value)
//    assert(flatMap.flatMap(flatMapIdentity) == flatMap)
