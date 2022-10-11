package byrd.riley.scalasimplemonadimpl

// F[_] represents a type that requires one type parameter (e.g. Option[_]).
trait Monad[F[_]] extends FlatMap[F] with Applicative[F]:
  // Define the major methods of Semigroupal, Apply, and Functor in terms of FlatMap and Applicative. This means that
  // the only thing that implementors have to do is define `flatMap` and `pure`.
  
  override def product[A, B](wrapper1: F[A], wrapper2: F[B]): F[TupleHelper.FlatConcat[A, B]] = {
    flatMap(wrapper1)(value1 =>
      flatMap(wrapper2) { value2 =>
        val tuple1: TupleHelper.IdentityTuple[value1.type] = TupleHelper.getIdentityTupleFor(value1)
        val tuple2: TupleHelper.IdentityTuple[value2.type] = TupleHelper.getIdentityTupleFor(value2)
        val flat1: TupleHelper.Flat[tuple1.type] = tuple1.flatten
        val flat2: TupleHelper.Flat[tuple2.type] = tuple2.flatten
        val flattenedProduct: Tuple.Concat[flat1.type, flat2.type] = flat1 ++ flat2
        pure(flattenedProduct.asInstanceOf[TupleHelper.FlatConcat[A, B]])
      }
    )
  }

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
  def apply[F[A]](using monad: Monad[F]): Monad[F] = monad
