package byrd.riley.scalasimplemonadimpl

import Id.Id

import scala.collection.mutable.ListBuffer

object MonadInstances:
  // Define some commonly used Monads.

  enum Maybe[+A] derives CanEqual:
    case Attested(value: A)
    case Unattested

  import Maybe.*

  given Maybe is Monad:
    extension [A](value: A)
      override def pure: Maybe[A] = Attested(value)
    extension [A](maybe: Maybe[A])
      override def flatMap[B](func: A => Maybe[B]): Maybe[B] =
        maybe match
          case Attested(value)   => func(value)
          case Unattested        => Unattested

  sealed trait LinkedList[+A]:
    def internalList: List[A]

  final case class LinkedCons[+A](head: A, tail: LinkedList[A]) extends LinkedList[A]:
    override val internalList: List[A] = scala.collection.immutable.::(head, tail.internalList)

  case object LinkedNil extends LinkedList[Nothing]:
    override val internalList: List[Nothing] = Nil

  object LinkedList:
    def apply[A](list: List[A]): LinkedList[A] =
      list.foldLeft(LinkedNil: LinkedList[A])((linkedList, elem) => LinkedCons(elem, linkedList))

  given LinkedList is Monad:
    extension [A](value: A)
      override def pure: LinkedList[A] = LinkedCons(value, LinkedNil)
    extension [A](list: LinkedList[A])
      override def flatMap[B](func: A => LinkedList[B]): LinkedList[B] =
        val listToReturn = new ListBuffer[B]

        list.internalList.foreach(elem =>
          func(elem).internalList.foreach(transformedElem =>
            listToReturn.addOne(transformedElem)
          )
        )

        LinkedList(listToReturn.toList)

  given Id is Monad:
    extension [A](value: A)
      override def pure: Id[A] = value
    extension [A](value: Id[A])
      override def flatMap[B](func: A => Id[B]): Id[B] = func(value)
      override def map[B](func: A => B): Id[B] = func(value)

  enum Disjunction[+E, +A]:
    case Sad(value: E)
    case Happy(value: A)

  object Disjunction:
    extension[E, A](disjunction: Disjunction[E, A])
      def isHappy: Boolean =
        disjunction match
          case Sad(_)   => false
          case Happy(_) => true
      def isSad: Boolean =
        disjunction match
          case Sad(_)   => true
          case Happy(_) => false

  import Disjunction.*

  // The Either Monad requires kind projection.
  given [E] => Disjunction[E, _] is Monad:
    extension [A](value: A)
      override def pure: Disjunction[E, A] = Happy(value)
    extension [A](disjunction: Disjunction[E, A])
      override def flatMap[B](func: A => Disjunction[E, B]): Disjunction[E, B] =
      disjunction match
        case Happy(value) => func(value)
        case Sad(value) => Sad(value)
