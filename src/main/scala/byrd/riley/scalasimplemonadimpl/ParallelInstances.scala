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

  given Parallel[LinkedList] = new Parallel[LinkedList]:
    import TupleHelper.flatten

    override type F[A] = ZipList[A]

    override def apply: Apply[ZipList] = new Apply[ZipList]:
      extension [A](zipList1: ZipList[A])
        override def product[B](zipList2: ZipList[B]): ZipList[FlatConcat[A, B]] =
          val zippedList = zipList1.internalList.zip(zipList2.internalList)
          val flattenedList: List[TupleHelper.FlatConcat[A, B]] = zippedList.map { tuple =>
            val tuple1: TupleHelper.IdentityTuple[tuple._1.type] = TupleHelper.getIdentityTupleFor(tuple._1)
            val tuple2: TupleHelper.IdentityTuple[tuple._2.type] = TupleHelper.getIdentityTupleFor(tuple._2)
            val flat1: TupleHelper.Flat[tuple1.type] = tuple1.flatten
            val flat2: TupleHelper.Flat[tuple2.type] = tuple2.flatten
            val flattenedProduct: Tuple.Concat[flat1.type, flat2.type] = flat1 ++ flat2

            flattenedProduct.asInstanceOf[TupleHelper.FlatConcat[A, B]]
          }
          ZipList(flattenedList)

      extension [A](zipList: ZipList[A])
        override def map[B](func: A => B): ZipList[B] = ZipList(zipList.internalList.map(func))

      extension [A, B](application: ZipList[A => B])
        override def ap(zipList: ZipList[A]): ZipList[B] = ZipList(application.internalList.lazyZip(zipList.internalList).map(_.apply(_)))


    import MonadInstances.given_Monad_LinkedList
    override def flatMap: FlatMap[LinkedList] = FlatMap[LinkedList]

    // override def parallel: LinkedList ~> ZipList = new (LinkedList ~> ZipList) { override def apply[A](value: LinkedList[A]): ZipList[A] = ZipList(value.internalList) }
    override def parallel[A]: LinkedList[A] => ZipList[A] = linkedList => ZipList(linkedList.internalList)

    // override def sequential: ZipList ~> LinkedList = new (ZipList ~> LinkedList) { override def apply[A](value: ZipList[A]): LinkedList[A] = LinkedList(value.internalList) }
    override def sequential[A]: ZipList[A] => LinkedList[A] = zippedList => LinkedList(zippedList.internalList)

  // parTupled for a Disjunction requires that the left type be a Semigroup because, unlike product, it does not fail fast.
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

    def toDisjunction: Disjunction[E, A] =
      this match
        case Invalid(error) => Sad(error)
        case Valid(value) => Happy(value)

    def withDisjunction[EE, B](func: Disjunction[E, A] => Disjunction[EE, B]): Validated[EE, B] = func(toDisjunction).toValidated

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

    extension[E, A](disjunction: Disjunction[E, A])
      def toValidated: Validated[E, A] =
        disjunction match
          case Sad(error) => Invalid(error)
          case Happy(value) => Valid(value)

  given[E: Semigroup]: Parallel[Disjunction[E, _]] = new Parallel[Disjunction[E, _]]:
    override type F[A] = Validated[E, A]

    override def apply: Apply[Validated[E, _]] =
      new Apply[Validated[E, _]]:
        extension [A](validated1: Validated[E, A])
          override def product[B](validated2: Validated[E, B]): Validated[E, FlatConcat[A, B]] = Validated.product(validated1, validated2)
        extension [A](validated: Validated[E, A])
          override def map[B](func: A => B): Validated[E, B] = validated.map(func)
        extension [A, B](application: Validated[E, A => B])
          override def ap(validated: Validated[E, A]): Validated[E, B] = validated.ap(application)

    import MonadInstances.given_Monad_Disjunction
    override def flatMap: FlatMap[Disjunction[E, _]] = FlatMap[Disjunction[E, _]]

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
