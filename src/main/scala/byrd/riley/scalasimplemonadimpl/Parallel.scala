package byrd.riley.scalasimplemonadimpl

trait Parallel[M[_]] {
  type F[_]

  // i.e. the parallel.
  def apply: Apply[F]
  // i.e. the sequential.
  def flatMap: FlatMap[M]

  // In Cats, written M ~> F or ~>[M, F]
  def parallel[A]: M[A] => F[A]
  def sequential[A]: F[A] => M[A]


  // In a parallel method, work occurs in a parallel but the input and output are sequentials.

  // Parallel Semigroupal methods
  def parProduct[A, B](wrapper1: M[A], wrapper2: M[B]): M[(A, B)] =
    sequential(apply.product(parallel(wrapper1), parallel(wrapper2)))
  def parTuple2[A, B](wrapper1: M[A], wrapper2: M[B]): M[(A, B)] =
    flatMap.map(parProduct(wrapper1, wrapper2)) { case (value1, value2) => (value1, value2) }
  def parTuple3[A, B, C](wrapper1: M[A], wrapper2: M[B], wrapper3: M[C]): M[(A, B, C)] =
    flatMap.map(parProduct(wrapper1, parProduct(wrapper2, wrapper3))) { case (value1, (value2, value3)) => (value1, value2, value3) }

  // Parallel Functor methods
  def parMap2[A, B, C](wrapper1: M[A], wrapper2: M[B])(func: (A, B) => C): M[C] =
    flatMap.map(parProduct(wrapper1, wrapper2)) { case (value1, value2) => func(value1, value2) }
  def parMap3[A, B, C, D](wrapper1: M[A], wrapper2: M[B], wrapper3: M[C])(func: (A, B, C) => D): M[D] =
    flatMap.map(parProduct(wrapper1, parProduct(wrapper2, wrapper3))) { case (value1, (value2, value3)) => func(value1, value2, value3) }

  // Parallel Apply methods
  def parAp[A, B](application: M[A => B])(wrapper: M[A]): M[B] =
    sequential(apply.ap(parallel(application))(parallel(wrapper)))
  def parAp2[A, B, C](application: M[(A, B) => C])(wrapper1: M[A], wrapper2: M[B]): M[C] =
    sequential(apply.ap2(parallel(application))(parallel(wrapper1), parallel(wrapper2)))
  def parAp3[A, B, C, D](application: M[(A, B, C) => D])(wrapper1: M[A], wrapper2: M[B], wrapper3: M[C]): M[D] =
    sequential(apply.ap3(parallel(application))(parallel(wrapper1), parallel(wrapper2), parallel(wrapper3)))
}

object Parallel {
  def apply[M[A]](implicit parallel: Parallel[M]): Parallel[M] = parallel

  // Parallel extension methods

  implicit class ParallelSemigroupalOps[M[_], A](wrapper1: M[A])(implicit parallel: Parallel[M]) {
    def parProduct[B](wrapper2: M[B]): M[(A, B)] = parallel.parProduct(wrapper1, wrapper2)
  }

  implicit class ParallelApplyOps[M[_], A, B](application: M[A => B])(implicit parallel: Parallel[M]) {
    def parAp(wrapper: M[A]): M[B] = parallel.parAp(application)(wrapper)
  }

  implicit class ParTuple2Ops[M[_], A, B](tuple2: (M[A], M[B]))(implicit parallel: Parallel[M]) {
    def parTupled: M[(A, B)] = parallel.parTuple2(tuple2._1, tuple2._2)
    def parMapN[C](func: (A, B) => C): M[C] = parallel.parMap2(tuple2._1, tuple2._2)(func)
    def parApWith[C](application: M[(A, B) => C]): M[C] = parallel.parAp2(application)(tuple2._1, tuple2._2)
  }
  implicit class ParTuple3Ops[M[_], A, B, C](tuple3: (M[A], M[B], M[C]))(implicit parallel: Parallel[M]) {
    def parTupled: M[(A, B, C)] = parallel.parTuple3(tuple3._1, tuple3._2, tuple3._3)
    def parMapN[D](func: (A, B, C) => D): M[D] = parallel.parMap3(tuple3._1, tuple3._2, tuple3._3)(func)
    def parApWith[D](application: M[(A, B, C) => D]): M[D] = parallel.parAp3(application)(tuple3._1, tuple3._2, tuple3._3)
  }
}