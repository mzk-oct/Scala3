package library.graph

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object LCADoubling:
  def apply[T <: Iterable[(Int, Int)]](graph: Array[T], root: Int): LCADoubling =
    val size = graph.length
    val depth = Array.fill(size){-1}
    val doubling = Array.fill(size){ArrayBuffer[Int]()}
    val stack = mutable.Stack[Int](root)
    depth(root) = 0
    while stack.nonEmpty do
      val top = stack.pop()
      for (next, _) <- graph(top) do
        if depth(next) == -1 then
          depth(next) = depth(top) + 1
          stack.push(next)
          val buffer = doubling(next)
          buffer.addOne(top)
          while buffer.size <= doubling(buffer.last).size do
            buffer.addOne(doubling(buffer.last)(buffer.size - 1))
    new LCADoubling(size, depth, doubling)

class LCADoubling private (val size: Int,
                           private val depth: Array[Int],
                           private val doubling: Array[ArrayBuffer[Int]]):
  def lca(a: Int, b: Int): Int =
    var i = a
    var j = b
    if depth(i) < depth(j) then
      val diff = depth(j) - depth(i)
      for idx <- doubling(j).indices do
        if ((diff >> idx) & 1) == 1 then
          j = doubling(j)(idx)
    if depth(j) < depth(i) then
      val diff = depth(i) - depth(j)
      for idx <- doubling(i).indices do
        if ((diff >> idx) & 1) == 1 then
          i = doubling(i)(idx)
    if i != j then
      for idx <- doubling(i).indices.reverse do
        if idx < doubling(i).size && doubling(i)(idx) != doubling(j)(idx) then
          i = doubling(i)(idx)
          j = doubling(j)(idx)
      doubling(i)(0)
    else
      i

