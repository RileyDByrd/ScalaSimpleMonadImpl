package byrd.riley.scalasimplemonadimpl

import byrd.riley.scalasimplemonadimpl.Id.Id

import scala.collection.mutable.ListBuffer

object MonadInstances {
  // Define some commonly used Monads.

  implicit def optionMonad: Monad[Option] = new Monad[Option] {
    override def pure[A](value: A): Option[A] = Some(value)

    override def flatMap[A, B](option: Option[A])(func: A => Option[B]): Option[B] =
      option match {
        case Some(value) => func(value)
        case None => None
      }
  }

  implicit def listMonad: Monad[List] = new Monad[List] {
    override def pure[A](value: A): List[A] = List(value)

    override def flatMap[A, B](value: List[A])(func: A => List[B]): List[B] = {
      val listToReturn = new ListBuffer[B]

      value.foreach(elem =>
        func(elem).foreach(transformedElem =>
          listToReturn.addOne(transformedElem)
        )
      )

      listToReturn.toList
    }
  }

  implicit def idMonad: Monad[Id] = new Monad[Id] {
    override def pure[A](value: A): Id[A] = value

    // Note that flatMap and map for Monad[Id] are identical.
    override def flatMap[A, B](value: Id[A])(func: A => Id[B]): Id[B] = func(value)

    override def map[A, B](value: Id[A])(func: A => B): Id[B] = func(value)
  }

  // The Either Monad requires kind projection. Much neater notation is provided by the kind projector plugin, which is
  // not used here.
  implicit def eitherMonad[C]: Monad[({type lam[Y] = Either[C, Y]})#lam] = new Monad[({type lam[Y] = Either[C, Y]})#lam] {
    override def pure[A](value: A): Either[C, A] = Right(value)
    override def flatMap[A, B](wrapper: Either[C, A])(func: A => Either[C, B]): Either[C, B] = wrapper match {
      case Right(value) => func(value)
      case Left(value) => Left(value)
    }
  }
}
