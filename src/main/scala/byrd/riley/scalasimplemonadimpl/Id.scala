package byrd.riley.scalasimplemonadimpl

object Id {
  type Id[A] = A
  def apply[A](value: A): Id[A] = value
}
