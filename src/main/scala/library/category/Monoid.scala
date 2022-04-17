package library.category



trait Monoid[T]:
  val zero: T
  def plus(left: T, right: T): T
  
