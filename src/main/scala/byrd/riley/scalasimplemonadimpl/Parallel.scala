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
  extension[A](flatMap1: M[A])
    def parProduct[B](flatMap2: M[B]): M[TupleHelper.FlatConcat[A, B]] =
      given Apply[F] = apply
      sequential(parallel(flatMap1).product(parallel(flatMap2)))

  // Parallel Apply method
  extension[A, B](application: M[A => B])
    def parAp(flatMap: M[A]): M[B] =
      given Apply[F] = apply
      sequential(parallel(application).ap(parallel(flatMap)))

  // Tuple operations
  extension [T <: Tuple](tuple: T)(using ev: Tuple.IsMappedBy[M][T])
    private def invertedTuple: M[Tuple.InverseMap[tuple.type, M]] = tuple.invertMap
    // The parallel part of parMapN, parTupled, and parApWith happens in invertedTuple, which means flatMap.map may be used. Nothing special would happen by using apply.map.
    def parMapN[A](func: Tuple.InverseMap[tuple.type, M] => A): M[A] =
      given FlatMap[M] = flatMap
      invertedTuple.map(func)
    def parTupled: M[Tuple.InverseMap[tuple.type, M]] = parMapN(identity)
    def parApWith[B](application: M[Tuple.InverseMap[tuple.type, M] => B]): M[B] =
      given FlatMap[M] = flatMap
      application.parProduct(invertedTuple).map {
        case (func: (Tuple.InverseMap[tuple.type, F] => B)) *: (tail: Tuple.InverseMap[tuple.type, M]) => func(tail).asInstanceOf[B]
      }

  // Determine the type of the tuple.
  // Brings tuples under one M through several parProduct-like operations. However many
  // apply.product calls there are, there will only be one call to sequential.
  extension[T <: Tuple: Tuple.IsMappedBy[M]](tuple: T)
    def invertMap: M[Tuple.InverseMap[tuple.type, M]] =
      given Parallel[M] = this
      val invertedMapParallel = Parallel.invertMapLoop[M, None.type](tuple, None).asInstanceOf[F[Tuple.InverseMap[tuple.type, M]]]
      sequential(invertedMapParallel)


object Parallel:
  def apply[M[A]](using parallel: Parallel[M]): Parallel[M] = parallel

  @tailrec
  private def invertMapLoop[M[_], Z](tuple: Tuple, carryOver: Option[Parallel[M]#F[Z]])(using parallel: Parallel[M]): Parallel[M]#F[Tuple.InverseMap[Tuple, M]] =
    given Apply[parallel.F] = parallel.apply

    tuple match
      case EmptyTuple => EmptyTuple.asInstanceOf[Parallel[M]#F[Tuple.InverseMap[Tuple, M]]]
      case (head: M[x]) *: EmptyTuple =>
        carryOver match
          case Some(value: parallel.F[Z]) => value.product(parallel.parallel(head)).asInstanceOf[Parallel[M]#F[Tuple.InverseMap[Tuple, M]]]
          case None => (parallel.parallel(head) *: EmptyTuple).asInstanceOf[Parallel[M]#F[Tuple.InverseMap[Tuple, M]]]
      case (head: M[x]) *: tail =>
        carryOver match
          case Some(value: parallel.F[Z]) =>
            val newCarryOver = Some(value.product[x](parallel.parallel(head)))
            invertMapLoop[M, TupleHelper.FlatConcat[Z, x]](tail, newCarryOver)
          case None =>
            val newCarryOver = Some(parallel.parallel(head))
            invertMapLoop[M, x](tail, newCarryOver)
