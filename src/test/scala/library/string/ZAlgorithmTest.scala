package library.string

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ZAlgorithmTest extends AnyFunSuite, Matchers:
  val random = scala.util.Random(0)
  for iteration <- 1 to 3000 do
    val length = random.nextInt(1000) + 1
    val str = Array.tabulate(length){_ => random.nextInt(2)}.mkString("")
    test(s"test case: $iteration, str: $str") {
      ZAlgorithm.longestCommonPrefixes(str) should be(impl(str))
    }
  for iteration <- 3001 to 4000 do
    val length = random.nextInt(300) + 1
    val str = Array.tabulate(length){_ => random.nextInt(10)}.mkString("")
    test(s"test case: $iteration, str: $str") {
      ZAlgorithm.longestCommonPrefixes(str) should be(impl(str))
    }

  def impl(str: String): Array[Int] =
    val result = Array.fill(str.length){0}
    for i <- str.indices do
      var j = 0
      while i + j < str.length && str(j) == str(i + j) do
        j += 1
      result(i) = j
    result
