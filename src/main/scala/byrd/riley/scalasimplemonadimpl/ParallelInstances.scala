package byrd.riley.scalasimplemonadimpl

import MonadInstances.Disjunction.{Happy, Sad}
import MonadInstances.{Disjunction, LinkedList}
import TupleHelper.FlatConcat

object ParallelInstances:
  // Define some commonly used Parallels.

  // parTupled for a List will zip the lists rather than find the cartesian product of them.
  sealed trait ZipList[+A]:
    def internalList: List[A]

  final case class ZipCons[+A](head: A, tail: ZipList[A]) extends ZipList[A]:
    override val internalList: List[A] = scala.collection.immutable.::(head, tail.internalList)

  case object ZipNil extends ZipList[Nothing]:
    override val internalList: List[Nothing] = Nil

  object ZipList:
    def apply[A](list: List[A]): ZipList[A] =
      list.foldLeft(ZipNil: ZipList[A])((linkedList, elem) => ZipCons(elem, linkedList))

  given LinkedList is Parallel:
    import TupleHelper.flatten

    override type Effect[F] = ZipList[F]

    override def apply: ZipList is Apply = new (ZipList is Apply):
      extension [A](zipList1: ZipList[A])
        override def product[B](zipList2: ZipList[B]): ZipList[FlatConcat[A, B]] =
          val zippedList = zipList1.internalList.zip(zipList2.internalList)
          val flattenedList: List[TupleHelper.FlatConcat[A, B]] = zippedList.map: tuple =>
            val tuple1: TupleHelper.IdentityTuple[tuple._1.type] = TupleHelper.getIdentityTupleFor(tuple._1)
            val tuple2: TupleHelper.IdentityTuple[tuple._2.type] = TupleHelper.getIdentityTupleFor(tuple._2)
            val flat1: TupleHelper.Flat[tuple1.type] = tuple1.flatten
            val flat2: TupleHelper.Flat[tuple2.type] = tuple2.flatten
            val flattenedProduct: Tuple.Concat[flat1.type, flat2.type] = flat1 ++ flat2

            flattenedProduct.asInstanceOf[TupleHelper.FlatConcat[A, B]]

          ZipList(flattenedList)

      extension [A](zipList: ZipList[A])
        override def map[B](func: A => B): ZipList[B] = ZipList(zipList.internalList.map(func))

      extension [A, B](application: ZipList[A => B])
        override def ap(zipList: ZipList[A]): ZipList[B] = ZipList(application.internalList.lazyZip(zipList.internalList).map(_.apply(_)))


    override def flatMap: LinkedList is FlatMap = MonadInstances.given_is_LinkedList_Monad

    // override def parallel: LinkedList ~> ZipList = new (LinkedList ~> ZipList) { override def apply[A](value: LinkedList[A]): ZipList[A] = ZipList(value.internalList) }
    override def parallel[A]: LinkedList[A] => ZipList[A] = linkedList => ZipList(linkedList.internalList)

    // override def sequential: ZipList ~> LinkedList = new (ZipList ~> LinkedList) { override def apply[A](value: ZipList[A]): LinkedList[A] = LinkedList(value.internalList) }
    override def sequential[A]: ZipList[A] => LinkedList[A] = zippedList => LinkedList(zippedList.internalList)

  // parTupled for a Disjunction requires that the left type be a Semigroup because, unlike product, it does not fail fast.
  // If it encounters more than one left, it will combine them as defined in Semigroup.combine. For rights, output
  // should be unchanged.
  enum Validated[+E, +A]:
    case Valid(value: A)
    case Invalid(value: E)

  object Validated:
    extension[E, A](validated: Validated[E, A])
      def isValid: Boolean =
        validated match
          case Invalid(_) => false
          case Valid(_)   => true
      def isInvalid: Boolean =
        validated match
          case Invalid(_) => true
          case Valid(_)   => false

      def toDisjunction: Disjunction[E, A] =
        validated match
          case Invalid(error) => Sad(error)
          case Valid(value)   => Happy(value)

      def withDisjunction[EE, B](func: Disjunction[E, A] => Disjunction[EE, B]): Validated[EE, B] =
        func(toDisjunction).toValidated

    extension[E, A](disjunction: Disjunction[E, A])
      def toValidated: Validated[E, A] =
        disjunction match
          case Sad(error) => Invalid(error)
          case Happy(value) => Valid(value)

  given [E: Semigroup as semigroup] => Disjunction[E, _] is Parallel:
    import Validated.*
    override type Effect[F] = Validated[E, F]

    override def apply: Validated[E, _] is Apply = new (Validated[E, _] is Apply):
      extension [A](validated1: Validated[E, A])
        override def product[B](validated2: Validated[E, B]): Validated[E, FlatConcat[A, B]] =
          (validated1, validated2) match
            case (Valid(thisValue), Valid(thatValue)) =>
              val tuple1: TupleHelper.IdentityTuple[thisValue.type] = TupleHelper.getIdentityTupleFor(thisValue)
              val tuple2: TupleHelper.IdentityTuple[thatValue.type] = TupleHelper.getIdentityTupleFor(thatValue)
              val flat1: TupleHelper.Flat[tuple1.type] = TupleHelper.flatten(tuple1)
              val flat2: TupleHelper.Flat[tuple2.type] = TupleHelper.flatten(tuple2)
              val flattenedProduct: Tuple.Concat[flat1.type, flat2.type] = flat1 ++ flat2

              Valid(flattenedProduct.asInstanceOf[FlatConcat[A, B]])

            case (Invalid(error1), Invalid(error2)) => Invalid(semigroup.combine(error1, error2))
            case (error @ Invalid(_), _) => error.asInstanceOf[Invalid[E, FlatConcat[A, B]]]
            case (_, error @ Invalid(_)) => error.asInstanceOf[Invalid[E, FlatConcat[A, B]]]

      extension [A](validated: Validated[E, A])
        override def map[B](func: A => B): Validated[E, B] =
          validated match
            case invalid @ Invalid(_) => invalid.asInstanceOf[Invalid[E, B]]
            case Valid(value)         => Valid(func(value))

      extension [A, B](application: Validated[E, A => B])
        override def ap(validated: Validated[E, A]): Validated[E, B] =
          (validated, application) match
            case (Valid(value), Valid(func)) => Valid(func(value))
            case (Invalid(error1), Invalid(error2)) => Invalid(semigroup.combine(error2, error1))
            case (error@Invalid(_), _) => error.asInstanceOf[Invalid[E, B]]
            case (_, error@Invalid(_)) => error.asInstanceOf[Invalid[E, B]]

    override def flatMap: Disjunction[E, _] is FlatMap = MonadInstances.given_is_Disjunction_Monad

    // override def parallel[A]: ({type lam[Y] = Disjunction[E, Y]})#lam[A] => ({type lam[Y] = Validated[E, Y]})#lam[A] = {
    //      new (({type lam[Y] = Disjunction[E, Y]})#lam[A] => ({type lam[Y] = Validated[E, Y]})#lam[A]) {
    //        override def apply(value: Disjunction[E, A]): Validated[E, A] = value.toValidated
    //      }
    //    }
    override def parallel[A]: Disjunction[E, A] => Validated[E, A] = _.toValidated

    // override def sequential: ~>[({type lam[Y] = Validated[E, Y]})#lam, ({type lam[Y] = Disjunction[E, Y]})#lam] =
    //      new (~>[({type lam[Y] = Validated[E, Y]})#lam, ({type lam[Y] = Disjunction[E, Y]})#lam]) {
    //        override def apply[A](value: Validated[E, A]): Disjunction[E, A] = value.toDisjunction
    //      }
    override def sequential[A]: Validated[E, A] => Disjunction[E, A] = _.toDisjunction
