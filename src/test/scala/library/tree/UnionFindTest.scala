package library.tree

import library.tree.UnionFind
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class UnionFindTest extends AnyFunSuite, Matchers {

  val random = scala.util.Random(0)
  for iteration <- 0 until 10000 do
    val size = random.nextInt(1000) + 1
    val sync = Sync(contestant = UnionFind(size), target = Impl(size))
    test(s"test case: $iteration, size: $size") {
      for _ <- 0 until 10000 do
        val a = random.nextInt(size)
        val b = random.nextInt(size)
        random.nextInt(5) match
          case op if op < 2 =>
            sync.unite(a, b)
          case op if op < 4 =>
            val (contestant, target) = sync.same(a, b)
            contestant should be (target)
          case _ =>
            val (contestant, target) = sync.sizeOf(a)
            contestant should be (target)
      sync.contestant.size should be (sync.target.size)
    }

  class Impl(val size: Int):
    private val group = (0 until size).toArray
    def same(a: Int, b: Int): Boolean = group(a) == group(b)
    def unite(a: Int, b: Int): Unit =
      if same(a, b) then return
      val oldRoot = group(a)
      val newRoot = group(b)
      for i <- group.indices do
        if group(i) == oldRoot then
          group(i) = newRoot
    def sizeOf(a: Int): Int = group.count(_ == group(a))

  trait UFT[T]:
    def unite(uft: T, a: Int, b: Int): Unit
    def same(uft: T, a: Int, b: Int): Boolean
    def sizeOf(uft: T, a: Int): Int

  given UFT[Impl] with
    override def unite(uft: Impl, a: Int, b: Int): Unit = uft.unite(a, b)
    override def same(uft: Impl, a: Int, b: Int): Boolean = uft.same(a, b)
    override def sizeOf(uft: Impl, a: Int) = uft.sizeOf(a)
  given UFT[UnionFind] with
    override def unite(uft: UnionFind, a: Int, b: Int) = uft.unite(a, b)
    override def same(uft: UnionFind, a: Int, b: Int) = uft.same(a, b)
    override def sizeOf(uft: UnionFind, a: Int) = uft.sizeOf(a)

  class Sync[Contestant: UFT, Target: UFT](val contestant: Contestant, val target: Target):
    def unite(a: Int, b: Int): Unit =
      summon[UFT[Contestant]].unite(contestant, a, b)
      summon[UFT[Target]].unite(target, a, b)
    def same(a: Int, b: Int): (Boolean, Boolean) =
      (
        summon[UFT[Contestant]].same(contestant, a, b),
        summon[UFT[Target]].same(target, a, b)
      )
    def sizeOf(a: Int): (Int, Int) =
      (
        summon[UFT[Contestant]].sizeOf(contestant, a),
        summon[UFT[Target]].sizeOf(target, a)
      )
}
