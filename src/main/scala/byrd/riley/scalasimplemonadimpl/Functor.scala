package byrd.riley.scalasimplemonadimpl

trait Functor[F[_]] {
  def map[A, B](wrapper: F[A])(func: A => B): F[B]

  def map2[A, B, C](wrapper1: F[A], wrapper2: F[B])(func: (A, B) => C): F[C]
  def map3[A, B, C, D](wrapper1: F[A], wrapper2: F[B], wrapper3: F[C])(func: (A, B, C) => D): F[D]
}

object Functor {
  def apply[F[_]](implicit functor: Functor[F]): Functor[F] = functor

  implicit class FunctorOps[F[_], A](wrapper: F[A])(implicit functor: Functor[F]) {
    def map[B](func: A => B): F[B] = functor.map(wrapper)(func)
  }
  implicit class FunctorTuple2Ops[F[_], A, B](tuple2: (F[A], F[B]))(implicit functor: Functor[F]) {
    def mapN[C](func: (A, B) => C): F[C] = functor.map2(tuple2._1, tuple2._2)(func)
  }
  implicit class FunctorTuple3Ops[F[_], A, B, C](tuple3: (F[A], F[B], F[C]))(implicit functor: Functor[F]) {
    def mapN[D](func: (A, B, C) => D): F[D] = functor.map3(tuple3._1, tuple3._2, tuple3._3)(func)
  }
}
