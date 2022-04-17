package library.tree

import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.TreeSet
import scala.util.Random
class PatriciaTest extends AnyFunSuite:
  val random = Random(0)
  for iteration <- 0 until 3000 do
    val seed = random.nextInt()
    val rng = Random(seed)
    test(s"iteration: $iteration, seed: $seed") {
      var pat = Patricia()
      var set = TreeSet[Int]()
      val temp = Array.fill(3000){rng.nextInt()}
      var count = 0
      for itr <- 0 until 30000 do
        rng.nextInt(5) match
          case 0 =>
            val value = rng.nextInt()
            pat = pat.inserted(value)
            set = set + value
            assert(pat.size == set.size)
            val pos = rng.nextInt(temp.length)
            temp(pos) = value
          case 1 =>
            val value = temp(rng.nextInt(temp.length))
            pat = pat.erased(value)
            set = set - value
            assert(pat.size == set.size)
          case _ =>
            count += 1
            val value = temp(rng.nextInt(temp.length))
            assert(pat.greaterThanOrEqual(value) == set.minAfter(value), s"count: $count, value: $value\n${pat.toArray.mkString("[", ",", "]")}\n${set.toArray.mkString("[", ",", "]")}")
            assert(pat.greaterThan(value) == (if value == Int.MaxValue then None else set.minAfter(value + 1)), s"count: $count, value: $value\n${pat.toArray.mkString("[", ",", "]")}\n${set.toArray.mkString("[", ",", "]")}")
            assert(pat.lessThan(value) == set.maxBefore(value), s"count: $count, value: $value\n${pat.toArray.mkString("[", ",", "]")}\n${set.toArray.mkString("[", ",", "]")}")
            assert(pat.lessThanOrEqual(value) == (if value == Int.MaxValue then if set.contains(Int.MaxValue) then Some(Int.MaxValue) else set.maxBefore(Int.MaxValue) else set.maxBefore(value + 1)), s"count: $count, value: $value\n${pat.toArray.mkString("[", ",", "]")}\n${set.toArray.mkString("[", ",", "]")}")
        if itr % 1000 == 0 then
          assert(pat.toArray sameElements set.toArray)
    }

