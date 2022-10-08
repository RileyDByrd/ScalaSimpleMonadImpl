package byrd.riley.scalasimplemonadimpl

import byrd.riley.scalasimplemonadimpl.Applicative.getIdentityTupleFor

// F[_] represents a type that requires one type parameter (e.g. Option[_]).
trait Monad[F[_]] extends FlatMap[F] with Applicative[F]:
  // Define the major methods of Semigroupal, Apply, and Functor in terms of FlatMap and Applicative. This means that
  // the only thing that implementors have to do is define `flatMap` and `pure`.
  
  override def product[A, B](wrapper1: F[A], wrapper2: F[B]): F[FlatConcat[A, B]] =
    flatMap(wrapper1)(value1 =>
      flatMap(wrapper2) { value2 =>
        val flattenedProduct = getIdentityTupleFor(value1).flatten ++ getIdentityTupleFor(value2).flatten
        flattenedProduct.asInstanceOf[FlatConcat[A, B]]
      }
    )

  override def ap[A, B](application: F[A => B])(wrapper: F[A]): F[B] =
    flatMap(application)((func: A => B) => flatMap(wrapper)(value => pure(func(value))))

  override def map[A, B](wrapper: F[A])(func: A => B): F[B] =
    flatMap(wrapper)(value => pure(func(value)))

  // Left identity: calling pure and transforming the result with func is the same as calling func.
  def leftIdentityLaw[A, B](value: A, func: A => F[B]): Unit =
    assert(flatMap(pure(value))(func) == func(value))

  // Right identity: passing pure to flatMap is the same as doing nothing.
  def rightIdentityLaw[A](wrapper: F[A]): Unit =
    assert(flatMap(wrapper)(pure) == wrapper)

object Monad:
  def apply[F[A]](implicit monad: Monad[F]): Monad[F] = monad

  implicit class MonadTupleOps[T <: Tuple](val tuple: T):
    def flatten: TupleTypeMatchers.Flat[tuple.type] =
      (tuple match
        case Tuple.EmptyTuple => tuple
        case head *: tail => head match
          case innerTuple: Tuple => innerTuple.flatten ++ tail.flatten
          case _ => head *: tail.flatten
        ).asInstanceOf[TupleTypeMatchers.Flat[tuple.type]]

    inline def flatMap[F[_] <: Tuple](f: [t] => t => F[t]): TupleTypeMatchers.FlatMap[tuple.type, F] = {
      val mapping: TupleTypeMatchers.Map[tuple.type, F] = tuple.map(f)
      val flatMapping: TupleTypeMatchers.FlatMap[tuple.type, F] = mapping.flatten.asInstanceOf[TupleTypeMatchers.FlatMap[tuple.type, F]]

      flatMapping
    }
