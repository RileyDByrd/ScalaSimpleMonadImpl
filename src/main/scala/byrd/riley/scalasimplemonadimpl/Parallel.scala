package byrd.riley.scalasimplemonadimpl

import scala.annotation.tailrec

trait Parallel[M[_]]:
  type F[_]

  // i.e. the parallel.
  def apply: Apply[F]
  // i.e. the sequential.
  def flatMap: FlatMap[M]

  // In Cats, written M ~> F or ~>[M, F]
  def parallel[A]: M[A] => F[A]
  def sequential[A]: F[A] => M[A]


  // In a parallel method, work occurs in a parallel but the input and output are sequentials.

  // Parallel Semigroupal method
  def parProduct[A, B](wrapper1: M[A], wrapper2: M[B]): M[TupleHelper.FlatConcat[A, B]] =
    sequential(apply.product(parallel(wrapper1), parallel(wrapper2)))

  // Parallel Apply method
  def parAp[A, B](application: M[A => B])(wrapper: M[A]): M[B] =
    sequential(apply.ap(parallel(application))(parallel(wrapper)))

object Parallel:
  def apply[M[A]](using parallel: Parallel[M]): Parallel[M] = parallel

  // Parallel extension methods
  extension[M[_], A](wrapper1: M[A])(using parallel: Parallel[M])
    def parProduct[B](wrapper2: M[B]): M[TupleHelper.FlatConcat[A, B]] = parallel.parProduct(wrapper1, wrapper2)

  extension[M[_], A, B](application: M[A => B])(using parallel: Parallel[M])
    def parAp(wrapper: M[A]): M[B] = parallel.parAp(application)(wrapper)

  // Brings tuples under one M through several parProduct-like operations. The difference is that, however many
  // apply.product calls there are, there will only be one call to sequential.
  def invertMap[M[_], T <: Tuple : Tuple.IsMappedBy[M]](tuple: T)(using parallel: Parallel[M]): M[Tuple.InverseMap[tuple.type, M]] = {
    val invertedMapParallel = invertMapLoop[M, None.type](tuple, None).asInstanceOf[parallel.F[Tuple.InverseMap[tuple.type, M]]]
    parallel.sequential(invertedMapParallel)
  }

  @tailrec
  private def invertMapLoop[M[_], Z](tuple: Tuple, carryOver: Option[Parallel[M]#F[Z]])(using parallel: Parallel[M]): Parallel[M]#F[Tuple.InverseMap[Tuple, M]] =
    tuple match
      case EmptyTuple => EmptyTuple.asInstanceOf[Parallel[M]#F[Tuple.InverseMap[Tuple, M]]]
      case (head: M[x]) *: EmptyTuple =>
        carryOver match
          case Some(value: parallel.F[Z]) => parallel.apply.product(value, parallel.parallel(head)).asInstanceOf[Parallel[M]#F[Tuple.InverseMap[Tuple, M]]]
          case None => (parallel.parallel(head) *: EmptyTuple).asInstanceOf[Parallel[M]#F[Tuple.InverseMap[Tuple, M]]]
      case (head: M[x]) *: tail =>
        carryOver match
          case Some(value: parallel.F[Z]) =>
            val newCarryOver = Some(parallel.apply.product[Z, x](value, parallel.parallel(head)))
            invertMapLoop[M, TupleHelper.FlatConcat[Z, x]](tail, newCarryOver)
          case None =>
            val newCarryOver = Some(parallel.parallel(head))
            invertMapLoop[M, x](tail, newCarryOver)

  extension[M[_], T <: Tuple](tuple: T)(using parallel: Parallel[M], ev: Tuple.IsMappedBy[M][T])
    private def invertedTuple: M[Tuple.InverseMap[tuple.type, M]] = invertMap(tuple)
    // The parallel part of parMapN, parTupled, and parApWith happens in invertedTuple, which means flatMap.map may be used. Nothing special would happen by using apply.map.
    def parMapN[A](func: Tuple.InverseMap[tuple.type, M] => A): M[A] = parallel.flatMap.map(invertedTuple)(func)
    def parTupled: M[Tuple.InverseMap[tuple.type, M]] = parMapN(identity)
    def parApWith[B](application: M[Tuple.InverseMap[tuple.type, M] => B]): M[B] =
      parallel.flatMap.map(parallel.parProduct(application, invertedTuple)) {
        case (func: (Tuple.InverseMap[tuple.type, parallel.F] => B)) *: (tail: Tuple.InverseMap[tuple.type, M]) => func(tail).asInstanceOf[B]
      }
