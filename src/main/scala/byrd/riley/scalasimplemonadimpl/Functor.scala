package byrd.riley.scalasimplemonadimpl

trait Functor[F[_]]:
  def map[A, B](wrapper: F[A])(func: A => B): F[B]


object Functor:
  def apply[F[_]](implicit functor: Functor[F]): Functor[F] = functor

  implicit class FunctorOps[F[_], A](wrapper: F[A])(implicit functor: Functor[F]):
    def map[B](func: A => B): F[B] = functor.map(wrapper)(func)
