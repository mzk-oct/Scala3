package library.tree

import library.category.Monoid
import library.tree.SegmentTree
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.reflect.ClassTag

class SegmentTreeTest extends AnyFunSuite, Matchers:

  given Monoid[Int] with
    override val zero = 0
    override def plus(left: Int, right: Int) = left + right
  val random = scala.util.Random(0)
  for iter <- 0 until 1000 do
    val size = random.nextInt(1000) + 1
    val tester = Tester(SegmentTree[Int](size), Impl[Int](size))
    test(s"test case: $iter, size: $size") {
      for _ <- 0 until scala.math.max(10000, size * 100) do
        val op = random.nextInt(2)
        op match
          case 0 =>
            val from = random.nextInt(size)
            val until = random.nextInt(size - from) + from + 1
            val (contestant, target) = tester.get(from, until)
            contestant should be (target)
          case 1 =>
            val position = random.nextInt(size)
            val value = random.nextInt(1000)
            tester.update(position, value)
      for _ <- 0 until 100 do
        val from = random.nextInt(size)
        val until = random.nextInt(size - from) + from + 1
        val (contestant, target) = tester.get(from, until)
        contestant should be (target)
      val (contestant, target) = tester.get(0, size)
      contestant should be (target)
    }

  class Impl[T: ClassTag](val size: Int)(using m: Monoid[T]):
    export m.*
    private val vec = Array.fill(size){zero}
    def get(from: Int, until: Int): T =
      (from until until).foldLeft(m.zero){(left, idx) => m.plus(left, vec(idx))}
    def changeValue(position: Int, value: T) =
      vec(position) = value

  trait Seg[T, Tree]:
    def apply(tree: Tree, from: Int, until: Int): T
    def update(tree: Tree, position: Int, value: T): Unit

  given Seg[Int, Impl[Int]] with
    override def apply(tree: Impl[Int], from: Int, until: Int): Int = tree.get(from, until)
    override def update(tree: Impl[Int], position: Int, value: Int) = tree.changeValue(position, value)

  given Seg[Int, SegmentTree[Int]] with
    override def apply(tree: SegmentTree[Int], from: Int, until: Int): Int = tree.get(from, until)
    override def update(tree: SegmentTree[Int], position: Int, value: Int) = tree.changeValue(position, value)

  class Tester[T, Contestant, Target](val contestant: Contestant, val target: Target)(using c: Seg[T, Contestant], t: Seg[T, Target]):
    def get(from: Int, until: Int): (T, T) = (c(contestant, from, until), t(target, from, until))
    def update(position: Int, value: T) =
      c.update(contestant, position, value)
      t.update(target, position, value)
