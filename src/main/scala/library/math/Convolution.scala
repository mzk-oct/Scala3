package library.math

import library.math.ModIntOpaque.*
object Convolution:
  import scala.math.*
  def ceilPow2(value: Int): Int =
    var x = 0
    while (1 << x) < value do
      x += 1
    x
  def bsf(value: Int): Int = Integer.numberOfTrailingZeros(value)
  inline def powMod(base: Int, exp: Int, inline mod: Int): Long =
    var result = 1L
    var b = base % mod
    var e = exp
    while e > 0 do
      if (e & 1) == 1 then
        result = result * b % mod
      b = b * b % mod
      e = e >> 1
    result
  inline def primitiveRoot(inline mod: Int): Int =
    inline if mod == 2 then 1
    else inline if mod == 167772161 then 3
    else inline if mod == 469762049 then 3
    else inline if mod == 754974721 then 11
    else inline if mod == 998244353 then 3
    else
      val divs = Array.fill(20){0}
      divs(0) = 2
      var count = 1
      var x = (mod - 1) / 2
      while x % 2 == 0 do x /= 2
      for i <- 3 to sqrt(x).toInt by 2 do
        if x % i == 0 then
          divs(count) = i
          count += 1
          while x % i == 0 do x /= i
      if x > 1 then
        divs(count) = x
        count += 1
      LazyList.from(2).find(g => (0 until count).forall(i => powMod(g, (mod - 1) / divs(i), mod) != 1)).get
  inline def apply()(using inline mod: Int): Convolution =
    val g = primitiveRoot(mod)
    val sumE = Array.fill(30){zero}
    val sumIE = Array.fill(30){zero}
    val es = Array.fill(30){zero}
    val ies = Array.fill(30){zero}
    val count = bsf(mod - 1)
    var e = g.toModInt.pow((mod - 1) >> count)
    var ie = e.inverse
    for i <- count to 2 by -1 do
      es(i - 2) = e
      ies(i - 2) = ie
      e *= e
      ie *= ie
    var now = one
    var inow = one
    for i <- 0 to count - 2 do
      sumE(i) = es(i) * now
      sumIE(i) = ies(i) * inow
      now *= ies(i)
      inow *= es(i)
    new Convolution(sumE, sumIE)

class Convolution(sumE: Array[ModInt], sumIE: Array[ModInt]):
  import library.math.Convolution.*
  inline def butterfly(a: Array[ModInt])(using inline mod: Int) =
    val h = ceilPow2(a.length)
    for ph <- 1 to h do
      val w = 1 << (ph - 1)
      val p = 1 << (h - ph)
      var now = one
      for s <- 0 until w do
        val offset = s << (h - ph + 1)
        for i <- 0 until p do
          val l = a(i + offset)
          val r = a(i + offset + p) * now
          a(i + offset) = l + r
          a(i + offset + p) = l - r
        now *= sumE(bsf(~s))
  inline def butterflyInv(a: Array[ModInt])(using inline mod: Int) =
    val h = ceilPow2(a.length)
    for ph <- h to 1 by -1 do
      val w = 1 << (ph - 1)
      val p = 1 << (h - ph)
      var now = one
      for s <- 0 until w do
        val offset = s << (h - ph + 1)
        for i <- 0 until p do
          val l = a(i + offset)
          val r = a(i + offset + p)
          a(i + offset) = l + r
          a(i + offset + p) = (l - r) * now
        now *= sumIE(bsf(~s))
  inline def smallConvolution(a: Array[ModInt], b: Array[ModInt])(using inline mod: Int): Array[ModInt] =
    if a.length == 0 || b.length == 0 then
      Array()
    else if a.length < b.length then
      convolutionBrute(b, a)
    else
      convolutionBrute(a, b)
  inline def largeConvolution(a: Array[ModInt], b: Array[ModInt])(using inline mod: Int): Array[ModInt] =
    val n = a.length
    val m = b.length
    val z = 1 << ceilPow2(n + m - 1)
    val az = Array.fill(z){zero}
    val bz = Array.fill(z){zero}
    a.copyToArray(az)
    b.copyToArray(bz)
    butterfly(az)
    butterfly(bz)
    for i <- az.indices do
      az(i) *= bz(i)
    butterflyInv(az)
    val inverse = z.toModInt.inverse
    val result = Array.tabulate(n + m - 1){az(_) * inverse}
    result
  inline def convolutionBrute(a: Array[ModInt], b: Array[ModInt])(using inline mod: Int): Array[ModInt] =
    val result = Array.fill(a.length + b.length - 1){zero}
    for i <- a.indices do
      for j <- b.indices do
        result(i + j) += a(i) * b(j)
    result
  inline def convolution(a: Array[ModInt], b: Array[ModInt])(using inline mod: Int): Array[ModInt] =
    if a.length < 60 || b.length < 60 then
      smallConvolution(a, b)
    else
      largeConvolution(a, b)