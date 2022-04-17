package library.math

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scala.util.*
import library.math.ModIntOpaque.*

class ConvolutionTest extends AnyFunSuite, Matchers {
  inline given mod: Int = 998244353
  val random = Random(0)
  val conv = Convolution()
  test("size: [200, 500], value: [0, mod)") {
    for iteration <- 0 until 50000 do
      val sizeA = random.between(200, 500)
      val sizeB = random.between(200, 500)
      val arrayA = Array.fill(sizeA){random.nextInt(mod).unsafeAsModInt}
      val arrayB = Array.fill(sizeB){random.nextInt(mod).unsafeAsModInt}
      val actual = conv.largeConvolution(arrayA, arrayB).map(_.value)
      val expected = conv.smallConvolution(arrayA, arrayB).map(_.value)
      actual should be(expected)
  }
}
