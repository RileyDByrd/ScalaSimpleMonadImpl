package byrd.riley.scalasimplemonadimpl

// F[_] represents a type that requires one type parameter (e.g. Option[_]).
trait Monad[F[_]] extends FlatMap[F], Applicative[F]:
  // Define the major methods of Semigroupal, Apply, and Functor in terms of FlatMap and Applicative. This means that
  // the only thing that implementors have to do is define `flatMap` and `pure`.
  
  extension[A](monad1: F[A])
    override def product[B](monad2: F[B]): F[TupleHelper.FlatConcat[A, B]] =
    monad1.flatMap: value1 =>
      monad2.flatMap: value2 =>
        val tuple1: TupleHelper.IdentityTuple[value1.type] = TupleHelper.getIdentityTupleFor(value1)
        val tuple2: TupleHelper.IdentityTuple[value2.type] = TupleHelper.getIdentityTupleFor(value2)
        val flat1: TupleHelper.Flat[tuple1.type] = tuple1.flatten
        val flat2: TupleHelper.Flat[tuple2.type] = tuple2.flatten
        val flattenedProduct: Tuple.Concat[flat1.type, flat2.type] = flat1 ++ flat2
        pure(flattenedProduct.asInstanceOf[TupleHelper.FlatConcat[A, B]])

    override def map[B](func: A => B): F[B] =
      monad1.flatMap(value => func(value).pure)
    
  extension[A, B](application: F[A => B])
    override def ap(monad: F[A]): F[B] =
      application.flatMap((func: A => B) => monad.flatMap(value => func(value).pure))
  
  // Left identity: calling pure and transforming the result with func is the same as calling func.
  // Very similar to identityApplyLaw.
  def leftIdentityMonadLaw[A, B](value: A, func: A => F[B]): Unit =
    assert(value.pure.flatMap(func) == func(value))

  // Right identity: passing pure to flatMap is the same as doing nothing.
  def rightIdentityMonadLaw[A](monad: F[A]): Unit =
    assert(monad.flatMap(_.pure) == monad)

  // Provide a cleaner implementation of the identityFlatMapLaw since pure is available.
  override def identityFlatMapLaw[A](flatMap: F[A]): Unit =
    rightIdentityMonadLaw(flatMap)
