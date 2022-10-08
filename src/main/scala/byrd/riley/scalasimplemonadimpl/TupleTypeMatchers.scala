package byrd.riley.scalasimplemonadimpl

import scala.Tuple.{Concat, Map}

object TupleTypeMatchers:
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
