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

  def invertMap[M[_] : Parallel, T <: Tuple : Tuple.IsMappedBy[M]](tuple: T): M[Tuple.InverseMap[tuple.type, M]] =
    invertMapLoop[M, None.type](tuple, None).asInstanceOf[M[Tuple.InverseMap[tuple.type, M]]]

  @tailrec
  private def invertMapLoop[M[_] : Parallel, Z](tuple: Tuple, carryOver: Option[M[Z]]): M[Tuple.InverseMap[Tuple, M]] =
    tuple match
      case EmptyTuple => EmptyTuple.asInstanceOf[M[Tuple.InverseMap[Tuple, M]]]
      case (head: M[x]) *: EmptyTuple =>
        carryOver match
          case Some(value: M[Z]) => Parallel[M].parProduct(value, head).asInstanceOf[M[Tuple.InverseMap[Tuple, M]]]
          case None => (head *: EmptyTuple).asInstanceOf[M[Tuple.InverseMap[Tuple, M]]]
      case (head: M[x]) *: tail =>
        carryOver match
          case Some(value: M[Z]) =>
            val newCarryOver = Some(Parallel[M].parProduct(value, head))
            invertMapLoop[M, TupleHelper.FlatConcat[Z, x]](tail, newCarryOver)
          case None =>
            val newCarryOver = Some(head)
            invertMapLoop[M, x](tail, newCarryOver)

  extension[M[_], T <: Tuple](tuple: T)(using Parallel[M], Tuple.IsMappedBy[M][T])
    private def invertedTuple: M[Tuple.InverseMap[tuple.type, M]] = invertMap(tuple)
    def parMapN[A](func: Tuple.InverseMap[tuple.type, M] => A): M[A] = Parallel[M].flatMap.map(invertedTuple)(func)
    def parTupled: M[Tuple.InverseMap[tuple.type, M]] = parMapN(identity)
    def parApWith[B](application: M[Tuple.InverseMap[tuple.type, M] => B]): M[B] = Parallel[M].flatMap.map(Parallel[M].parProduct(application, invertedTuple)) {
      case (func: (Tuple.InverseMap[tuple.type, M] => B)) *: (tail: Tuple.InverseMap[tuple.type, M]) => func(tail).asInstanceOf[B]
    }
