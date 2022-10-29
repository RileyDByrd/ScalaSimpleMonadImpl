package byrd.riley.scalasimplemonadimpl

import scala.annotation.tailrec

// "Applicative" is short for "Applicative Functor".
trait Applicative[F[_]] extends Apply[F]:
  // i.e. wrap
  def pure[A](value: A): F[A]

  // A `pure` of the value inside a wrapper is equal to the wrapper instance.
  def identityLaw[A](wrapper: F[A]): Unit =
    assert(ap(pure((value: A) => value))(wrapper) == wrapper)

object Applicative:
  def apply[F[_]](using applicative: Applicative[F]): Applicative[F] = applicative

  // Does not override .pure if a class has already implemented it, which means Monad instead of MyMonad will be used.
  extension[F[_], A](value: A)(using applicative: Applicative[F])
    def pure: F[A] = applicative.pure(value)

  def invertMap[F[_] : Applicative, T <: Tuple : Tuple.IsMappedBy[F]](tuple: T): F[Tuple.InverseMap[tuple.type, F]] =
    invertMapLoop[F, None.type](tuple, None).asInstanceOf[F[Tuple.InverseMap[tuple.type, F]]]

  @tailrec
  private def invertMapLoop[F[_], Z](tuple: Tuple, carryOver: Option[F[Z]])(using applicative: Applicative[F]): F[Tuple.InverseMap[Tuple, F]] =
    tuple match
      case EmptyTuple => EmptyTuple.asInstanceOf[F[Tuple.InverseMap[Tuple, F]]]
      case (head: F[x]) *: EmptyTuple =>
        carryOver match
          case Some(value: F[Z]) => applicative.product(value, head).asInstanceOf[F[Tuple.InverseMap[Tuple, F]]]
          case None => (head *: EmptyTuple).asInstanceOf[F[Tuple.InverseMap[Tuple, F]]]
      case (head: F[x]) *: tail =>
        carryOver match
          case Some(value: F[Z]) =>
            val newCarryOver = Some(applicative.product(value, head))
            invertMapLoop[F, TupleHelper.FlatConcat[Z, x]](tail, newCarryOver)
          case None =>
            val newCarryOver = Some(head)
            invertMapLoop[F, x](tail, newCarryOver)

  // Applicative functors are for working on multiple structures. Thus, all tuple operations are here.
  extension[F[_], T <: Tuple](tuple: T)(using applicative: Applicative[F], ev: Tuple.IsMappedBy[F][T])
    private def invertedTuple: F[Tuple.InverseMap[tuple.type, F]] = invertMap(tuple)

    def mapN[A](func: Tuple.InverseMap[tuple.type, F] => A): F[A] =
      applicative.map(invertedTuple)(func)

    def tupled: F[Tuple.InverseMap[tuple.type, F]] =
      mapN(identity)

    def apWith[B](application: F[Tuple.InverseMap[tuple.type, F] => B]): F[B] =
      applicative.map(applicative.product(application, invertedTuple)) {
        case (func: (Tuple.InverseMap[tuple.type, F] => B)) *: (tail: Tuple.InverseMap[tuple.type, F]) => func(tail).asInstanceOf[B]
      }
