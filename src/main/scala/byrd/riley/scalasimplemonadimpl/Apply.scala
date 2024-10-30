package byrd.riley.scalasimplemonadimpl

import scala.annotation.tailrec

// Simpler Applicative Functor
trait Apply extends Semigroupal, Functor:
  // We don't use "apply" because it means something else in Scala, so we use "ap".
  // Any one of product, ap, and map may be defined in terms of the other two.
  extension[A, B](application: Self[A => B])
    def ap(apply: Self[A]): Self[B]

  // The identity function applied to an Apply is equal to the Apply.
//  def identityApplyLaw[A](apply: F[A]): Unit =
//    assert(apply.map(_ => identity[A]).ap(apply) == apply)

  // Applies are for working on multiple structures. Thus, all tuple operations are here.
  extension [T <: Tuple](tuple: T)(using Tuple.IsMappedBy[Self][T])
    private def invertedTuple: Self[Tuple.InverseMap[tuple.type, Self]] = tuple.invertMap

    def mapN[A](func: Tuple.InverseMap[tuple.type, Self] => A): Self[A] =
      invertedTuple.map(func)

    def tupled: Self[Tuple.InverseMap[tuple.type, Self]] =
      mapN(identity)

    def apWith[B](application: Self[Tuple.InverseMap[tuple.type, Self] => B]): Self[B] =
      application.product(invertedTuple).map:
        case (func: (Tuple.InverseMap[tuple.type, Self] => B)) *: (tail: Tuple.InverseMap[tuple.type, Self]) => func(tail).asInstanceOf[B]

  // Determine the type of the tuple.
  extension[T <: Tuple: Tuple.IsMappedBy[Self]](tuple: T)
    def invertMap: Self[Tuple.InverseMap[tuple.type, Self]] =
      given Self is Apply = this
      Apply.invertMapLoop[Self, None.type](tuple, None).asInstanceOf[Self[Tuple.InverseMap[tuple.type, Self]]]


object Apply:
  import TupleHelper.given_CanEqual_EmptyTuple_Tuple
  import TupleHelper.given_CanEqual_Tuple_EmptyTuple
  
  @tailrec
  private def invertMapLoop[F[_], Z](tuple: Tuple, carryOver: Option[F[Z]])(using a: F is Apply): F[Tuple.InverseMap[Tuple, F]] =
    tuple match
      case EmptyTuple => EmptyTuple.asInstanceOf[F[Tuple.InverseMap[Tuple, F]]]
      case (head: F[x]) *: EmptyTuple =>
        carryOver match
          case Some(value: F[Z]) => value.product(head).asInstanceOf[F[Tuple.InverseMap[Tuple, F]]]
          case None => (head *: EmptyTuple).asInstanceOf[F[Tuple.InverseMap[Tuple, F]]]
      case (head: F[x]) *: tail =>
        carryOver match
          case Some(value: F[Z]) =>
            val newCarryOver = Some(value.product(head))
            invertMapLoop[F, TupleHelper.FlatConcat[Z, x]](tail, newCarryOver)
          case None =>
            val newCarryOver = Some(head)
            invertMapLoop[F, x](tail, newCarryOver)