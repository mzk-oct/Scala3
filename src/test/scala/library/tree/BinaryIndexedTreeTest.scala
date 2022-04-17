package library.tree

import library.category.Monoid
import library.tree.BinaryIndexedTree
import org.scalatest.matchers.should.Matchers

import scala.language.postfixOps
import scala.reflect.ClassTag
import scala.util.Random

class BinaryIndexedTreeTest extends org.scalatest.funsuite.AnyFunSuite, Matchers:

  given Monoid[Int] with
    override val zero = 0
    override def plus(left: Int, right: Int) = left + right
  val generator = Random(0)
  val testCase = 1000
  for _ <- 0 until testCase do
    val seed = generator.nextInt()
    val rand = Random(seed)
    val size = rand.nextInt(1000) + 1
    val tester = Tester(contestant = BinaryIndexedTree[Int](size), target = Impl[Int](size))
    test(s"test case seed: $seed, size: $size") {
      for _ <- 0 until 1000 do
        rand.nextInt(2) match
          case 0 =>
            val position = rand.nextInt(size)
            val (contestant, target) = tester.get(position)
            contestant should be (target)
          case 1 =>
            val position = rand.nextInt(size)
            val value = rand.nextInt(1000)
            tester.update(position, value)
      val (contestant, target) = tester.enumerate
      contestant should be (target)
    }

  class Impl[T <: AnyVal : ClassTag](val size: Int)(using m: Monoid[T]):
    private val vec = Array.fill(size){m.zero}
    def get(position: Int): T =
      var result = m.zero
      for i <- 0 to position do
        result = m.plus(result, vec(i))
      result
    def add(position: Int, value: T): Unit =
      vec(position) = m.plus(vec(position), value)

  trait Bit[T <: AnyVal, B]:
    def get(bit: B, position: Int): T
    def add(bit: B, position: Int, value: T): Unit
    def size(bit: B): Int
  given Bit[Int, Impl[Int]] with
    override def get(bit: Impl[Int], position: Int): Int = bit.get(position)
    override def add(bit: Impl[Int], position: Int, value: Int): Unit = bit.add(position, value)
    override def size(bit: Impl[Int]): Int = bit.size
  given Bit[Int, BinaryIndexedTree[Int]] with
    override def get(bit: BinaryIndexedTree[Int], position: Int) = bit.get(position)
    override def add(bit: BinaryIndexedTree[Int], position: Int, value: Int) = bit.add(position, value)
    override def size(bit: BinaryIndexedTree[Int]) = bit.size

  class Tester[T <: AnyVal, Contestant, Target](val contestant: Contestant, val target: Target)(using Bit[T, Contestant], Bit[T, Target]):
    val bitC = summon[Bit[T, Contestant]]
    val bitT = summon[Bit[T, Target]]
    def get(position: Int): (T, T) =
      (bitC.get(contestant, position), bitT.get(target, position))
    def update(position: Int, value: T): Unit =
      bitC.add(contestant, position, value)
      bitT.add(target, position, value)
    def enumerate: (IndexedSeq[T], IndexedSeq[T]) =
      ((0 until bitC.size(contestant)).map(bitC.get(contestant, _)), (0 until bitT.size(target)).map(bitT.get(target, _)))
