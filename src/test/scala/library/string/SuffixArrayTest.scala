package library.string

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SuffixArrayTest extends AnyFunSuite, Matchers:
  val random = scala.util.Random(0)
  val chars = ('a' to 'z').mkString("") + ('A' to 'Z').mkString("") + ('0' to '9').mkString("")
  for iteration <- 1 to 3000 do
    val size = random.nextInt(1000) + 1
    val str = Array.tabulate(size){_ => chars(random.nextInt(chars.length))}.mkString("")
    test(s"suffix array test case: $iteration, str: $str") {
      val target = impl(str)
      val short = SuffixArray.suffixArrayShort(str)
      val mid = SuffixArray.suffixArrayMid(str)
      val long = SuffixArray.suffixArrayLong(str)
      short should equal(target)
      mid should equal(target)
      long should equal(target)
    }
  for iteration <- 1 to 1000 do
    val size = random.nextInt(100) + 1
    val str = Array.tabulate(size){_ => random.nextInt(2)}.mkString("")
    test(s"suffix array test case: $iteration, str: $str") {
      val target = impl(str)
      val short = SuffixArray.suffixArrayShort(str)
      val mid = SuffixArray.suffixArrayMid(str)
      val long = SuffixArray.suffixArrayLong(str)
      short should equal(target)
      mid should equal(target)
      long should equal(target)
    }
  for iteration <- 1 to 1000 do
    val size = random.nextInt(100) + 1
    val str = Array.tabulate(size){_ => random.nextInt(3)}.mkString("")
    test(s"lcp test case: $iteration, str: $str") {
      val sa = SuffixArray.suffixArray(str)
      val target = impl(str, sa)
      val contestant = SuffixArray.lcpArray(str, sa)
      contestant should be (target)
    }


  extension (a: Array[Int])
    def contentEqual(b: Array[Int]): Boolean =
      a.length == b.length && a.indices.forall(i => a(i) == b(i))

  def impl(str: String, suffixArray: Array[Int]): Array[Int] =
    if str.length <= 1 then return Array()
    val result = Array.fill(str.length - 1){0}
    for i <- result.indices do
      val p = suffixArray(i)
      val q = suffixArray(i + 1)
      var len = 0
      while p + len < str.length && q + len < str.length && str(p + len) == str(q + len) do
        len += 1
      result(i) = len
    result
  def impl(str: String): Array[Int] =
    val suffix = str.tails.toArray
    str.indices.sortBy(suffix).toArray

