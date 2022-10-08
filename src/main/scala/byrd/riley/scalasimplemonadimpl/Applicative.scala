package byrd.riley.scalasimplemonadimpl

// "Applicative" is short for "Applicative Functor".
trait Applicative[F[_]] extends Apply[F]:
  // i.e. wrap
  def pure[A](value: A): F[A]

  // A `pure` of the value inside a wrapper is equal to the wrapper instance.
  def identityLaw[A](wrapper: F[A]): Unit =
    assert(ap(pure((value: A) => value))(wrapper) == wrapper)

object Applicative:
  def apply[F[_]](implicit applicative: Applicative[F]): Applicative[F] = applicative

  def getIdentityTupleFor[A](value: A): TupleTypeMatchers.IdentityTuple[A] =
    value match
      case tuple: Tuple => tuple
      case _ => Tuple1(value)

  // Does not override .pure if a class has already implemented it, which means Monad instead of MyMonad will be used.
  implicit class ApplicativeOps[F[_], A](value: A)(implicit applicative: Applicative[F]):
    def pure: F[A] = applicative.pure(value)

  def inverseMap[F[_], X](tuple: Tuple)(implicit ev: Tuple.IsMappedBy[F][X], applicator: Apply[F]): Tuple.InverseMap[tuple.type, F] =
    inverseMap(tuple, applicator).asInstanceOf[Tuple.InverseMap[tuple.type, F]]

  def inverseMap[F[_]](tuple: Tuple, applicator: Apply[F]): Tuple =
    tuple match
      case EmptyTuple => EmptyTuple
      case head *: tail => applicator.product(head, inverseMap(tail))

  // Applicative functors are for working on multiple structures. Thus, all tuple operations are here.
  implicit class ApplicativeTupleOps[F[_], X](val tuple: Tuple)(implicit ev: Tuple.IsMappedBy[F][X]):
    val invertedTuple: Tuple.InverseMap[tuple.type, F] = inverseMap(tuple)

    def mapN[A, B](func: Tuple.InverseMap[tuple.type, F] => B)(implicit applicative: Applicative[F]): F[B] =
      applicative.pure(func(invertedTuple))

    def tupled(implicit applicative: Applicative[F]): F[Tuple.InverseMap[tuple.type, F]] =
      mapN(identity)

    def apWith[B](application: F[Tuple.InverseMap[tuple.type, F] => B])(implicit applicator: Apply[F]): F[B] =
      applicator.map(application)(func => func(invertedTuple))