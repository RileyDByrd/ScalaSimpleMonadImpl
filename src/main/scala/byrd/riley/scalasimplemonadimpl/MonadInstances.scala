package byrd.riley.scalasimplemonadimpl

import Id.Id

import scala.collection.mutable.ListBuffer

object MonadInstances:
  // Define some commonly used Monads.

  enum Maybe[+A]:
    case Attested(value: A)
    case Unattested

  import Maybe.*

  given Monad[Maybe] = new Monad[Maybe]:
    extension [A](value: A)
      override def pure: Maybe[A] = Attested(value)
    extension [A](option: Maybe[A])
      override def flatMap[B](func: A => Maybe[B]): Maybe[B] =
        option match
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

  given Monad[LinkedList] = new Monad[LinkedList]:
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

  given Monad[Id] = new Monad[Id]:
    extension [A](value: A)
      override def pure: Id[A] = value
    extension [A](value: Id[A])
      override def flatMap[B](func: A => Id[B]): Id[B] = func(value)
      override def map[B](func: A => B): Id[B] = func(value)

  enum Disjunction[+A, +B]:
    case Sad(value: A)
    case Happy(value: B)
  
  import Disjunction.*
    
  // The Either Monad requires kind projection.
  given[C]: Monad[Disjunction[C, _]] = new Monad[Disjunction[C, _]]:
    extension [A](value: A)
      override def pure: Disjunction[C, A] = Happy(value)
    extension [A](either: Disjunction[C, A])
      override def flatMap[B](func: A => Disjunction[C, B]): Disjunction[C, B] =
      either match
        case Happy(value) => func(value)
        case Sad(value) => Sad(value)
