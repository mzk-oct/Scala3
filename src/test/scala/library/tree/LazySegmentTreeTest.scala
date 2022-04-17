package library.tree

import library.category.Monoid
import library.tree.{LazySegmentTree, Operation}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import sun.text.UCompactIntArray

import scala.math.*
import scala.util.Random
class LazySegmentTreeTest extends AnyFunSuite, Matchers {

  val random = Random(3)
  extension (r: Random)
    def nextRange(size: Int): (Int, Int) =
      val a = r.nextInt(size)
      val b = r.nextInt(size)
      (min(a, b), max(a, b) + 1)
  val valueLimit = 1000
  val maxSize = 1000
  for iteration <- 0 until 3000 do
    val size = random.nextInt(maxSize) + 1
    val sync = Sync(size)
    test(s"test case: $iteration, size: $size") {
      for _ <- 0 until 10000 do
        random.nextInt(8) match
          case 0 =>
            val position = random.nextInt(size)
            val (contestant, target) = sync.get(position)
            contestant should be (target)
          case 1 =>
            val (from, until) = random.nextRange(size)
            val (contestant, target) = sync.get(from, until)
            contestant should be (target)
          case 2 =>
            val position = random.nextInt(size)
            val value = random.nextInt(valueLimit)
            sync.update(position, value)
          case 3 =>
            val position = random.nextInt(size)
            val diff = random.nextInt(valueLimit)
            sync.apply(position, diff)
          case _ =>
            val (from, until) = random.nextRange(size)
            val diff = random.nextInt(valueLimit)
            sync.apply(from, until, diff)
      (0 until size).foreach{i =>
        sync.get(i, i + 1) should be (sync.get(i))
      }
    }


  given Operation[Int, Int] with
    override val zero = 0
    override def apply(operation: Int, operand: Int) = operation + operand
    override def merge(newOperation: Int, oldOperation: Int) = newOperation + oldOperation
  given Monoid[Int] with
    override val zero = 0
    override def plus(left: Int, right: Int) = scala.math.max(left, right)

  class Impl(val size: Int)(using t: Monoid[Int], f: Operation[Int, Int]):
    private val vec = Array.fill(size){t.zero}
    def get(from: Int, until: Int): Int = (from until until).foldLeft(t.zero){(left, idx) => t.plus(left, vec(idx))}
    def get(position: Int): Int = vec(position)
    def apply(from: Int, until: Int, diff: Int) =
      for i <- from until until do
        vec(i) += diff
    def apply(position: Int, diff: Int) =
      vec(position) += diff
    def update(position: Int, value: Int) =
      vec(position) = value

  trait LazySeg[T]:
    def get(from: Int, until: Int): T
    def get(position: Int): T
    def apply(from: Int, until: Int, diff: Int): Unit
    def apply(position: Int, diff: Int): Unit
    def update(position: Int, value: Int): Unit
  class Target(seg: Impl) extends LazySeg[Int]:
    export seg.*
    def this(size: Int) =
      this(Impl(size))
  class Contestant(seg: LazySegmentTree[Int, Int]) extends LazySeg[Int]:
    export seg.*
    def this(size: Int) =
      this(LazySegmentTree[Int, Int](size))

  class Sync(contestant: Contestant, target: Target):
    def this(size: Int) =
      this(Contestant(size), Target(size))
    def get(position: Int): (Int, Int) = (contestant.get(position), target.get(position))
    def get(from: Int, until: Int): (Int, Int) = (contestant.get(from, until), target.get(from, until))
    def apply(from: Int, until: Int, diff: Int) =
      contestant.apply(from, until, diff)
      target.apply(from, until, diff)
    def apply(position: Int, diff: Int) =
      contestant.apply(position, diff)
      target.apply(position, diff)
    def update(position: Int, value: Int) =
      contestant.update(position, value)
      target.update(position, value)
}
