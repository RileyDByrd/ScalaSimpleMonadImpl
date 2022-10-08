package byrd.riley.scalasimplemonadimpl

trait Apply[F[_]] extends Semigroupal[F] with Functor[F]:
  // We don't use "apply" because it means something else in Scala, so we use "ap".
  def ap[A, B](application: F[A => B])(wrapper: F[A]): F[B]
  // Any one of product, ap, and map may be defined in terms of the other two.

object Apply:
  def apply[F[A]](implicit apply: Apply[F]): Apply[F] = apply

  implicit class ApplyOps[F[_], A, B](application: F[A => B])(implicit apply: Apply[F]):
    def ap(wrapper: F[A]): F[B] = apply.ap(application)(wrapper)
  