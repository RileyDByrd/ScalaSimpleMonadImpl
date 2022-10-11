package byrd.riley.scalasimplemonadimpl

trait Functor[F[_]]:
  def map[A, B](wrapper: F[A])(func: A => B): F[B]


object Functor:
  def apply[F[_]](using functor: Functor[F]): Functor[F] = functor

  extension[F[_], A](wrapper: F[A])(using functor: Functor[F])
    def map[B](func: A => B): F[B] = functor.map(wrapper)(func)
