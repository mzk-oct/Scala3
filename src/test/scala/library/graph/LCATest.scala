package library.graph

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import library.tree.*

import scala.annotation.tailrec
import scala.collection.mutable

class LCATest extends AnyFunSuite, Matchers:
  type Tree = Array[ArrayBuffer[Int]]
  val random = Random(4)
  for iteration <- 0 until 1000 do
    val size = random.between(100, 10000)
    val seed = random.nextInt()
    val tree = makeTree(seed, size)
    val oracle = LCAEulerTour(tree.map(_.map((_, 0))), 0)
    val contestant = LCADoubling(tree.map(_.map((_, 0))), 0)
    test(s"case: $iteration, size: $size, seed: $seed") {
      for _ <- 0 until size * 4 do
        val a = random.between(0, size)
        val b = random.between(0, size)
        val actual = contestant.lca(a, b)
        val expected = oracle.lca(a, b)
        actual should be (expected)
    }

  def makeTree(seed: Int, size: Int): Tree =
    val uft = UnionFind(size)
    val random = Random(seed)
    val result = Array.fill(size){ArrayBuffer[Int]()}
    for _ <- 0 until size << 2 do
      val a = random.nextInt(size - 1)
      val b = random.nextInt(size - 1)
      if !uft.same(a, b) then
        uft.unite(a, b)
        result(a).addOne(b)
        result(b).addOne(a)
    if uft.sizeOf(0) < size then
      for i <- 1 until size do
        if !uft.same(0, i) then
          uft.unite(0, i)
          result(0).addOne(i)
          result(i).addOne(0)
    result

  final class Oracle private (size: Int):
    private val depth = Array.fill(size){-1}
    private val parent = Array.fill(size){-1}
    def this(tree: Tree, root: Int) =
      this(tree.length)
      val stack = mutable.Stack[Int](root)
      depth(root) = 0
      while stack.nonEmpty do
        val top = stack.pop()
        for next <- tree(top) do
          if depth(next) == -1 then
            depth(next) = depth(top) + 1
            parent(next) = top
            stack.push(next)
    @tailrec def lca(a: Int, b: Int): Int =
      if a == b then
        a
      else if depth(a) == depth(b) then
        lca(parent(a), parent(b))
      else if depth(a) < depth(b) then
        lca(a, parent(b))
      else
        lca(parent(a), b)

