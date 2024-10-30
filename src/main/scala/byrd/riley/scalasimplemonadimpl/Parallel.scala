package byrd.riley.scalasimplemonadimpl

import scala.annotation.tailrec

trait Parallel:
  type Self[M]
  type SelfAlias[M] = Self[M]
  type Effect[F]

  // i.e. the sequential.
  //noinspection ApparentResultTypeRefinement
  def flatMap: FlatMap { type Self[M] = SelfAlias[M] }

  // i.e. the parallel.
  //noinspection ApparentResultTypeRefinement
  def apply: Apply { type Self[F] = Effect[F] }

  // In Cats, written M ~> F or ~>[M, F]
  def sequential[A]: Effect[A] => Self[A]
  def parallel[A]: Self[A] => Effect[A]


  // In a parallel method, work occurs in a parallel but the input and output are sequentials.

  // Parallel Semigroupal method
  extension[A](flatMap1: Self[A])
    def parProduct[B](flatMap2: Self[B]): Self[TupleHelper.FlatConcat[A, B]] =
      given Effect is Apply = apply
      sequential(parallel(flatMap1).product(parallel(flatMap2)))

  // Parallel Apply method
  extension[A, B](application: Self[A => B])
    def parAp(flatMap: Self[A]): Self[B] =
      given Effect is Apply = apply
      sequential(parallel(application).ap(parallel(flatMap)))

  // Tuple operations
  extension [T <: Tuple](tuple: T)(using Tuple.IsMappedBy[Self][T])
    private def invertedTuple: Self[Tuple.InverseMap[tuple.type, Self]] = tuple.invertMap
    // The parallel part of parMapN, parTupled, and parApWith happens in invertedTuple, which means flatMap.map may be used. Nothing special would happen by using apply.map.
    def parMapN[A](func: Tuple.InverseMap[tuple.type, Self] => A): Self[A] =
      given Self is FlatMap = flatMap
      invertedTuple.map(func)

    def parTupled: Self[Tuple.InverseMap[tuple.type, Self]] = parMapN(identity)

    def parApWith[B](application: Self[Tuple.InverseMap[tuple.type, Self] => B]): Self[B] =
      given Self is FlatMap = flatMap
      application.parProduct(invertedTuple).map:
        case (func: (Tuple.InverseMap[tuple.type, Effect] => B)) *: (tail: Tuple.InverseMap[tuple.type, Self]) => func(tail).asInstanceOf[B]

// Determine the type of the tuple.
// Brings tuples under one M through several parProduct-like operations. However many
// apply.product calls there are, there will only be one call to sequential.
  extension[T <: Tuple: Tuple.IsMappedBy[Self]](tuple: T)
    def invertMap: Self[Tuple.InverseMap[tuple.type, Self]] =
      given Self is Parallel = this
      val invertedMapParallel = Parallel.invertMapLoop[Self, None.type](tuple, None).asInstanceOf[Effect[Tuple.InverseMap[tuple.type, Self]]]
      sequential(invertedMapParallel)


object Parallel:
  import TupleHelper.given_CanEqual_EmptyTuple_Tuple
  import TupleHelper.given_CanEqual_Tuple_EmptyTuple
  
  @tailrec
  private def invertMapLoop[M[_], Z](tuple: Tuple, carryOver: Option[(M is Parallel)#Effect[Z]])(using parallel: M is Parallel): (M is Parallel)#Effect[Tuple.InverseMap[Tuple, M]] =
    given parallel.Effect is Apply = parallel.apply

    tuple match
      case EmptyTuple => EmptyTuple.asInstanceOf[(M is Parallel)#Effect[Tuple.InverseMap[Tuple, M]]]
      case (head: M[x]) *: EmptyTuple =>
        carryOver match
          case Some(value: parallel.Effect[Z]) => value.product(parallel.parallel(head)).asInstanceOf[(M is Parallel)#Effect[Tuple.InverseMap[Tuple, M]]]
          case None => (parallel.parallel(head) *: EmptyTuple).asInstanceOf[(M is Parallel)#Effect[Tuple.InverseMap[Tuple, M]]]
      case (head: M[x]) *: tail =>
        carryOver match
          case Some(value: parallel.Effect[Z]) =>
            val newCarryOver = Some(value.product[x](parallel.parallel(head)))
            invertMapLoop[M, TupleHelper.FlatConcat[Z, x]](tail, newCarryOver)
          case None =>
            val newCarryOver = Some(parallel.parallel(head))
            invertMapLoop[M, x](tail, newCarryOver)
