package byrd.riley.scalasimplemonadimpl

// Delete and replace with Algebraic Datatypes import once published.
trait Semigroup[A]:
  def combine(x: A, y: A): A

  def associativeLaw(x: A, y: A, z: A): Unit =
    assert(combine(x, combine(y, z)) == combine(combine(x, y), z))
