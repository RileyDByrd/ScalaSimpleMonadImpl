package byrd.riley.scalasimplemonadimpl

// Delete and replace with Algebraic Datatypes import once published.
trait Semigroup:
  type Self
  def combine(x: Self, y: Self): Self

//  def associativeLaw(x: Self, y: Self, z: Self): Unit =
//    assert(combine(x, combine(y, z)) == combine(combine(x, y), z))
