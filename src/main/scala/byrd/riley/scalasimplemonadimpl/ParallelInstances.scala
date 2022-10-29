package byrd.riley.scalasimplemonadimpl

import byrd.riley.scalasimplemonadimpl.TupleHelper.FlatConcat

object ParallelInstances:
  // Define some commonly used Parallels.

  // parTupled for a List will zip the lists rather than find the cartesian product of them.
  class ZipList[A](val value: List[A]) extends AnyVal
  object ZipList:
    def apply[A](value: List[A]): ZipList[A] = new ZipList(value)

  given Parallel[List] = new Parallel[List]:
    import TupleHelper.flatten

    override type F[A] = ZipList[A]

    override def apply: Apply[ZipList] = new Apply[ZipList]:
      override def product[A, B](wrapper1: ZipList[A], wrapper2: ZipList[B]): ZipList[TupleHelper.FlatConcat[A, B]] = {
        val zippedList = wrapper1.value.zip(wrapper2.value)
        val flattenedList: List[TupleHelper.FlatConcat[A, B]] = zippedList.map { tuple =>
          val tuple1: TupleHelper.IdentityTuple[tuple._1.type] = TupleHelper.getIdentityTupleFor(tuple._1)
          val tuple2: TupleHelper.IdentityTuple[tuple._2.type] = TupleHelper.getIdentityTupleFor(tuple._2)
          val flat1: TupleHelper.Flat[tuple1.type] = tuple1.flatten
          val flat2: TupleHelper.Flat[tuple2.type] = tuple2.flatten
          val flattenedProduct: Tuple.Concat[flat1.type, flat2.type] = flat1 ++ flat2

          flattenedProduct.asInstanceOf[TupleHelper.FlatConcat[A, B]]
        }
        ZipList(flattenedList)
      }

      override def map[A, B](wrapper: ZipList[A])(func: A => B): ZipList[B] =
        ZipList(wrapper.value.map(func))

      override def ap[A, B](application: ZipList[A => B])(wrapper: ZipList[A]): ZipList[B] =
        ZipList(application.value.lazyZip(wrapper.value).map(_.apply(_)))

    import MonadInstances.given_Monad_List
    override def flatMap: FlatMap[List] = FlatMap[List]

    // override def parallel: List ~> ZipList = new (List ~> ZipList) { override def apply[A](value: List[A]): ZipList[A] = new ZipList(value) }
    override def parallel[A]: List[A] => ZipList[A] = new ZipList(_)

    // override def sequential: ZipList ~> List = new (ZipList ~> List) { override def apply[A](value: ZipList[A]): List[A] = value.value }
    override def sequential[A]: ZipList[A] => List[A] = _.value

  // parTupled for an Either requires that the left type be a Semigroup because, unlike product, it does not fail fast.
  // If it encounters more than one left, it will combine them as defined in Semigroup.combine. For rights, output
  // should be unchanged.
  import Validated._
  class Validated[+E, +A]:
    def isValid: Boolean =
      this match
        case Invalid(_) => false
        case _ => true
    def isInvalid: Boolean =
      this match
        case Invalid(_) => true
        case _ => false

    def map[B](func: A => B): Validated[E, B] =
      this match
        case i @ Invalid(_) => i
        case Valid(value) => Valid(func(value))

    def ap[EE >: E, B](other: Validated[EE, A => B])(using EE: Semigroup[EE]): Validated[EE, B] =
      (this, other) match
        case (Valid(value), Valid(func)) => Valid(func(value))
        case (Invalid(error1), Invalid(error2)) => Invalid(EE.combine(error2, error1))
        case (error @ Invalid(_), _) => error
        case (_, error @ Invalid(_)) => error

    def toEither: Either[E, A] =
      this match
        case Invalid(error) => Left(error)
        case Valid(value) => Right(value)

    def withEither[EE, B](func: Either[E, A] => Either[EE, B]): Validated[EE, B] = Validated.fromEither(func(toEither))

  object Validated:
    case class Valid[+A](value: A) extends Validated[Nothing, A]
    case class Invalid[+E](value: E) extends Validated[E, Nothing]

    def product[EE, A, B](first: Validated[EE, A], second: Validated[EE, B])(using EE: Semigroup[EE]): Validated[EE, TupleHelper.FlatConcat[A, B]] =
      (first, second) match
        case (Valid(thisValue), Valid(thatValue)) =>
          val tuple1: TupleHelper.IdentityTuple[thisValue.type] = TupleHelper.getIdentityTupleFor(thisValue)
          val tuple2: TupleHelper.IdentityTuple[thatValue.type] = TupleHelper.getIdentityTupleFor(thatValue)
          val flat1: TupleHelper.Flat[tuple1.type] = TupleHelper.flatten(tuple1)
          val flat2: TupleHelper.Flat[tuple2.type] = TupleHelper.flatten(tuple2)
          val flattenedProduct: Tuple.Concat[flat1.type, flat2.type] = flat1 ++ flat2

          Valid(flattenedProduct.asInstanceOf[FlatConcat[A, B]])

        case (Invalid(error1), Invalid(error2)) => Invalid(EE.combine(error1, error2))
        case (error@Invalid(_), _) => error
        case (_, error@Invalid(_)) => error

    def fromEither[E, A](either: Either[E, A]): Validated[E, A] = either.fold(Invalid(_), Valid(_))

    extension[E, A](either: Either[E, A])
      def toValidated: Validated[E, A] =
        either match
          case Left(error) => Invalid(error)
          case Right(value) => Valid(value)

  given given_Parallel_Either[E: Semigroup]: Parallel[Either[E, _]] = new Parallel[Either[E, _]]:
    override type F[A] = Validated[E, A]

    override def apply: Apply[Validated[E, _]] =
      new Apply[Validated[E, _]]:
        override def product[A, B](wrapper1: Validated[E, A], wrapper2: Validated[E, B]): Validated[E, TupleHelper.FlatConcat[A, B]] =
          Validated.product(wrapper1, wrapper2)

        override def map[A, B](wrapper: Validated[E, A])(func: A => B): Validated[E, B] =
          wrapper.map(func)

        override def ap[A, B](application: Validated[E, A => B])(wrapper: Validated[E, A]): Validated[E, B] =
          wrapper.ap(application)

    import MonadInstances.given_Monad_Either
    override def flatMap: FlatMap[Either[E, _]] = FlatMap[Either[E, _]]

    // override def parallel[A]: ({type lam[Y] = Either[E, Y]})#lam[A] => ({type lam[Y] = Validated[E, Y]})#lam[A] = {
    //      new (({type lam[Y] = Either[E, Y]})#lam[A] => ({type lam[Y] = Validated[E, Y]})#lam[A]) {
    //        override def apply(value: Either[E, A]): Validated[E, A] = value.toValidated
    //      }
    //    }
    override def parallel[A]: Either[E, A] => Validated[E, A] = _.toValidated

    // override def sequential: ~>[({type lam[Y] = Validated[E, Y]})#lam, ({type lam[Y] = Either[E, Y]})#lam] =
    //      new (~>[({type lam[Y] = Validated[E, Y]})#lam, ({type lam[Y] = Either[E, Y]})#lam]) {
    //        override def apply[A](value: Validated[E, A]): Either[E, A] = value.toEither
    //      }
    override def sequential[A]: Validated[E, A] => Either[E, A] = _.toEither
