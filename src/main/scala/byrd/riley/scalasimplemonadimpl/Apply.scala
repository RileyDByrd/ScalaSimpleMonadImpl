package byrd.riley.scalasimplemonadimpl

import scala.annotation.tailrec

// Simpler Applicative Functor
trait Apply[F[_]] extends Semigroupal[F], Functor[F]:
  // We don't use "apply" because it means something else in Scala, so we use "ap".
  // Any one of product, ap, and map may be defined in terms of the other two.
  extension[A, B](application: F[A => B])
    def ap(apply: F[A]): F[B]

  // The identity function applied to an Apply is equal to the Apply.
  def identityApplyLaw[A](apply: F[A]): Unit =
    assert(apply.map(_ => identity[A]).ap(apply) == apply)

  // Applies are for working on multiple structures. Thus, all tuple operations are here.
  extension [T <: Tuple](tuple: T)(using ev: Tuple.IsMappedBy[F][T])
    private def invertedTuple: F[Tuple.InverseMap[tuple.type, F]] = tuple.invertMap

    def mapN[A](func: Tuple.InverseMap[tuple.type, F] => A): F[A] =
      invertedTuple.map(func)

    def tupled: F[Tuple.InverseMap[tuple.type, F]] =
      mapN(identity)

    def apWith[B](application: F[Tuple.InverseMap[tuple.type, F] => B]): F[B] =
      application.product(invertedTuple).map {
        case (func: (Tuple.InverseMap[tuple.type, F] => B)) *: (tail: Tuple.InverseMap[tuple.type, F]) => func(tail).asInstanceOf[B]
      }

  // Determine the type of the tuple.
  extension[T <: Tuple: Tuple.IsMappedBy[F]](tuple: T)
    def invertMap: F[Tuple.InverseMap[tuple.type, F]] =
      given Apply[F] = this
      Apply.invertMapLoop[F, None.type](tuple, None).asInstanceOf[F[Tuple.InverseMap[tuple.type, F]]]


object Apply:
  def apply[F[A]: Apply](using apply: Apply[F]): Apply[F] = apply

  @tailrec
  private def invertMapLoop[F[_]: Apply, Z](tuple: Tuple, carryOver: Option[F[Z]]): F[Tuple.InverseMap[Tuple, F]] =
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