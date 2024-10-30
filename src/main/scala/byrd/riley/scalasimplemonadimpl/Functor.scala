package byrd.riley.scalasimplemonadimpl

trait Functor:
  type Self[A]
  extension[A] (functor: Self[A])
    def map[B](func: A => B): Self[B]

  // Mapping a value to itself should yield an equal functor.
//  def identityFunctorLaw[A](functor: F[A]): Unit =
//    assert(functor.map(identity) == functor)

  // Associativity: mapping over f and then mapping over g is the same as mapping over two functions f and g.
//  def associativeFunctorLaw[A, B, C](functor: F[A], funcF: A => B, funcG: B => C): Unit =
//    assert(functor.map(funcF).map(funcG) == functor.map(value => funcG(funcF(value))))
