package library.category


trait SemiRing[T] extends Monoid[T]:
  def plus(left: T, right: T): T
  def times(left: T, right: T): T
  val zero: T
  val one: T

object SemiRing:
  import library.math.ModIntOpaque.*
  inline def semiRing()(using inline mod: Int): SemiRing[ModInt] =
    new SemiRing[ModInt]:
      override val zero = 0.toModInt
      override val one = 1.toModInt
      override def plus(left: ModInt, right: ModInt) = left + right
      override def times(left: ModInt, right: ModInt) = left * right
