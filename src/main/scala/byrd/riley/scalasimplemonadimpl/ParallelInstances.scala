package byrd.riley.scalasimplemonadimpl

object ParallelInstances {
  // Define some commonly used Parallels.

  class ZipList[A](val value: List[A]) extends AnyVal
  object ZipList { def apply[A](value: List[A]): ZipList[A] = new ZipList(value) }
  implicit def listParallel: Parallel[List] = new Parallel[List] {
    override type F[A] = ZipList[A]

    override def apply: Apply[ZipList] = new Apply[ZipList] {
      override def product[A, B](wrapper1: ZipList[A], wrapper2: ZipList[B]): ZipList[(A, B)] =
        ZipList(wrapper1.value.zip(wrapper2.value))

      override def map[A, B](wrapper: ZipList[A])(func: A => B): ZipList[B] =
        ZipList(wrapper.value.map(func))

      override def ap[A, B](application: ZipList[A => B])(wrapper: ZipList[A]): ZipList[B] =
        ZipList(application.value.lazyZip(wrapper.value).map(_.apply(_)))
    }

    import MonadInstances.listMonad
    override def flatMap: FlatMap[List] = FlatMap[List]

    // override def parallel: List ~> ZipList = new (List ~> ZipList) { override def apply[A](value: List[A]): ZipList[A] = new ZipList(value) }
    override def parallel[A]: List[A] => ZipList[A] = new ZipList(_)

    // override def sequential: ZipList ~> List = new (ZipList ~> List) { override def apply[A](value: ZipList[A]): List[A] = value.value }
    override def sequential[A]: ZipList[A] => List[A] = _.value
  }

  import Validated._
  class Validated[+E, +A] {
    def isValid: Boolean =
      this match {
        case Invalid(_) => false
        case _ => true
      }
    def isInvalid: Boolean =
      this match {
        case Invalid(_) => true
        case _ => false
      }

    def product[EE >: E, B](other: Validated[EE, B])(implicit EE: Semigroup[EE]): Validated[EE, (A, B)] =
      (this, other) match {
        case (Valid(thisValue), Valid(thatValue)) => Valid((thisValue, thatValue))
        case (Invalid(error1), Invalid(error2)) => Invalid(EE.combine(error1, error2))
        case (error @ Invalid(_), _) => error
        case (_, error @ Invalid(_)) => error
      }

    def map[B](func: A => B): Validated[E, B] =
      this match {
        case i @ Invalid(_) => i
        case Valid(value) => Valid(func(value))
      }

    def ap[EE >: E, B](other: Validated[EE, A => B])(implicit EE: Semigroup[EE]): Validated[EE, B] =
      (this, other) match {
        case (Valid(value), Valid(func)) => Valid(func(value))
        case (Invalid(error1), Invalid(error2)) => Invalid(EE.combine(error2, error1))
        case (error @ Invalid(_), _) => error
        case (_, error @ Invalid(_)) => error
      }

    def toEither: Either[E, A] = this match {
      case Invalid(error) => Left(error)
      case Valid(value) => Right(value)
    }

    def withEither[EE, B](func: Either[E, A] => Either[EE, B]): Validated[EE, B] = Validated.fromEither(func(toEither))
  }
  object Validated {
    case class Valid[+A](value: A) extends Validated[Nothing, A]
    case class Invalid[+E](value: E) extends Validated[E, Nothing]

    def fromEither[E, A](either: Either[E, A]): Validated[E, A] = either.fold(Invalid(_), Valid(_))

    implicit class EitherOpsFromValidated[E, A](either: Either[E, A]) {
      def toValidated: Validated[E, A] = either match {
        case Left(error) => Invalid(error)
        case Right(value) => Valid(value)
      }
    }
  }
  implicit def eitherParallel[E: Semigroup]: Parallel[({type lam[Y] = Either[E, Y]})#lam] = new Parallel[({type lam[Y] = Either[E, Y]})#lam] {
    override type F[A] = Validated[E, A]

    override def apply: Apply[({type lam[Y] = Validated[E, Y]})#lam] =
      new Apply[({type lam[Y] = Validated[E, Y]})#lam] {
        override def product[A, B](wrapper1: Validated[E, A], wrapper2: Validated[E, B]): Validated[E, (A, B)] =
          wrapper1.product(wrapper2)

        override def map[A, B](wrapper: Validated[E, A])(func: A => B): Validated[E, B] =
          wrapper.map(func)

        override def ap[A, B](application: Validated[E, A => B])(wrapper: Validated[E, A]): Validated[E, B] =
          wrapper.ap(application)
      }

    import MonadInstances.eitherMonad
    override def flatMap: FlatMap[({type lam[Y] = Either[E, Y]})#lam] = FlatMap[({type lam[Y] = Either[E, Y]})#lam]

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
  }
}
