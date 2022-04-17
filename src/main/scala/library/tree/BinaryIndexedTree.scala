package library.tree

import library.category.Monoid

import scala.reflect.ClassTag

class BinaryIndexedTree[T : ClassTag](val size: Int)(using m: Monoid[T]):
  export m.*
  private val vec = Array.fill(size){zero}
  def get(position: Int): T =
    var result = zero
    var pos = position
    while vec.indices.contains(pos) do
      result = plus(result, vec(pos))
      pos -= ~pos & (pos + 1)
    result
  def add(position: Int, value: T): Unit =
    var pos = position
    while vec.indices.contains(pos) do
      vec(pos) = plus(vec(pos), value)
      pos += ~pos & (pos + 1)





