package library.graph

import library.tree.SegmentTree

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
object LCAEulerTour:
  def apply[T <: Iterable[(Int, Int)]](graph: Array[T], root: Int): LCAEulerTour =
    val size = graph.length
    val depth = Array.fill(size){-1}
    val inTime = Array.fill(size){-1}
    val outTime = Array.fill(size){-1}
    val stack = mutable.Stack(root)
    val tour = Array.fill(2 * size - 1){-1}
    depth(root) = 0
    var time = 0
    while stack.nonEmpty do
      val top = stack.pop()
      tour(time) = top
      outTime(top) = time
      if inTime(top) == -1 then
        inTime(top) = time
        for (next, _) <- graph(top) do
          if depth(next) == -1 then
            depth(next) = depth(top) + 1
            stack.push(top)
            stack.push(next)
      time += 1
    given library.category.Monoid[Int] with
      override val zero = -1
      override def plus(left: Int, right: Int) =
        if left == zero then right
        else if right == zero then left
        else if depth(left) < depth(right) then left
        else right
    val lcaTree = SegmentTree(tour)
    new LCAEulerTour(inTime, outTime, lcaTree)

class LCAEulerTour private (private val inTime: Array[Int], private val outTime: Array[Int], private val lcaTree: SegmentTree[Int]):
  import scala.math._
  def lca(a: Int, b: Int): Int =
    lcaTree.get(min(inTime(a), inTime(b)), max(outTime(a), outTime(b)) + 1)

