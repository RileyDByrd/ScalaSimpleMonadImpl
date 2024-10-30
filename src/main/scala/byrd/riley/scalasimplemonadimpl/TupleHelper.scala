package byrd.riley.scalasimplemonadimpl

import scala.CanEqual.derived
import scala.Tuple.{ Concat, Map }

object TupleHelper:
  type Flat[T <: Tuple] <: Tuple =
    T match
      case EmptyTuple => EmptyTuple
      case head *: tail => head match
        case Tuple => Concat[Flat[head], Flat[tail]]
        case _ => head *: Flat[tail]

  type FlatMap[T <: Tuple, F[_]] = Flat[Map[T, F]]

  type IdentityTuple[T] <: Tuple =
    T match
      case Tuple => T & Tuple
      case _ => Tuple1[T]

  type FlatConcat[A, B] = Concat[Flat[IdentityTuple[A]], Flat[IdentityTuple[B]]]

  given CanEqual[EmptyTuple, Tuple] = derived
  given CanEqual[Tuple, EmptyTuple] = derived
  
  // These functions do not rely on any Category class.
  def getIdentityTupleFor[A](value: A): TupleHelper.IdentityTuple[A] =
    value match
      case tuple: Tuple => tuple
      case _ => Tuple1(value)
  
  extension[T <: Tuple] (tuple: T)
    def flatten: Flat[tuple.type] =
      tuple match
        case EmptyTuple => tuple.asInstanceOf[Flat[tuple.type]]
        case head *: tail =>
          head match
            case innerTuple: Tuple => (innerTuple.flatten ++ tail.flatten).asInstanceOf[Flat[tuple.type]]
            case _ => (head *: tail.flatten).asInstanceOf[Flat[tuple.type]]

    inline def flatMap[F[_] <: Tuple](f: [t] => t => F[t]): FlatMap[tuple.type, F] =
      val mapping: Tuple.Map[tuple.type, F] = tuple.map(f)
      val flatMapping: FlatMap[tuple.type, F] = mapping.flatten.asInstanceOf[FlatMap[tuple.type, F]]

      flatMapping
