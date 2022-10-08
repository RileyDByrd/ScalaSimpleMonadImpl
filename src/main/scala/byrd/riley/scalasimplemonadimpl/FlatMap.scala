package byrd.riley.scalasimplemonadimpl

trait FlatMap[F[_]] extends Apply[F]:
  def flatMap[A, B](wrapper: F[A])(func: A => F[B]): F[B]
  def flatten[A](doubleWrapper: F[F[A]]): F[A] = flatMap(doubleWrapper)(wrapper => wrapper)

  // Associativity: flatMapping over two functions f and g is the same as flatMapping over f
  // and then flatMapping over g.
  def associativeLaw[A, B, C](wrapper: F[A], funcF: A => F[B], funcG: B => F[C]): Unit =
    assert(flatMap(flatMap(wrapper)(funcF))(funcG) == flatMap(wrapper)(value => flatMap(funcF(value))(funcG)))

object FlatMap:
  def apply[F[A]](implicit flatMap: FlatMap[F]): FlatMap[F] = flatMap

  implicit class FlatMapOps[F[_], A](wrapper: F[A])(implicit flatMap: FlatMap[F]):
    def flatMap[B](func: A => F[B]): F[B] = flatMap.flatMap(wrapper)(func)

  implicit class FlattenOps[F[_], A](doubleWrapper: F[F[A]])(implicit flatMap: FlatMap[F]):
    def flatten: F[A] = flatMap.flatten(doubleWrapper)
