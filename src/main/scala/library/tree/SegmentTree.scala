package library.tree

import library.category.Monoid
import library.tree.SegmentTree.ceilLog2

import scala.reflect.ClassTag

class SegmentTree[T: ClassTag] private (
                                         val size: Int,
                                         private val half: Int,
                                         private val vec: Array[T])(using m: Monoid[T]):
  private def this(size: Int, half: Int)(using m: Monoid[T]) =
    this(size, half, Array.fill(2 << half){m.zero})
  def this(size: Int)(using Monoid[T]) =
    this(size, ceilLog2(size))
  def this(initial: Array[T])(using m: Monoid[T]) =
    this(initial.length)
    for i <- initial.indices do
      vec(i + (1 << half)) = initial(i)
    for i <- (1 << half) - 1 to 1 by - 1 do
      vec(i) = m.plus(vec(i << 1), vec((i << 1) + 1))
  def get(from: Int, until: Int): T =
    var left = from + (1 << half)
    var right = until + (1 << half)
    var leftSum = m.zero
    var rightSum = m.zero
    while left < right do
      if (left & 1) == 1 then
        leftSum = m.plus(leftSum, vec(left))
        left += 1
      if (right & 1) == 1 then
        right -= 1
        rightSum = m.plus(vec(right), rightSum)
      left >>= 1
      right >>= 1
    m.plus(leftSum, rightSum)
  def changeValue(position: Int, value: T) =
    var pos = position + (1 << half)
    vec(pos) = value
    pos >>= 1
    while pos > 0 do
      vec(pos) = m.plus(vec(pos * 2), vec(pos * 2 + 1))
      pos >>= 1
  def applyValue(position: Int, value: T) =
    var pos = position + (1 << half)
    vec(pos) = m.plus(vec(pos), value)
    pos >>= 1
    while pos > 0 do
      vec(pos) = m.plus(vec(pos * 2), vec(pos * 2 + 1))
      pos >>= 1

object SegmentTree:
  def ceilLog2(value: Int): Int =
    var result = 0
    for d <- 4 to 0 by -1 do
      if value >= (1 << (result + (1 << d))) then
        result += 1 << d
    if (1 << result) == value then
      result
    else
      result + 1

