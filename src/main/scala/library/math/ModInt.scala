package library.math

class ModInt(val rawValue: Long) extends AnyVal:
  inline def +(right: ModInt)(using inline mod: Int): ModInt = ModInt((rawValue + right.rawValue) % mod)
  inline def -(right: ModInt)(using inline mod: Int): ModInt = ModInt((rawValue - right.rawValue) % mod)
  inline def *(right: ModInt)(using inline mod: Int): ModInt = ModInt((rawValue * right.rawValue) % mod)
  inline def /(right: ModInt)(using inline mod: Int): ModInt = this * right.inverse
  inline def pow(exp: Long)(using inline mod: Int): ModInt =
    var result = 1L
    var base = rawValue
    var e = exp
    while e > 0 do
      if (e & 1) == 1 then
        result = result * base % mod
      base = base * base % mod
      e >>= 1
    ModInt(result)
  inline def pow(exp: Int)(using inline mod: Int): ModInt = pow(exp.toLong)
  inline def inverse(using inline mod: Int): ModInt =
    var x = rawValue
    var y = mod.toLong
    var a = 1L
    var b = 0L
    var c = 0L
    var d = 1L
    while y != 0 do
      val q = x / y
      val e = a - c * q
      val f = b - d * q
      a = c
      b = d
      c = e
      d = f
      x = y
      y = c * rawValue + d * mod
    require(x.abs == 1L)
    ModInt(if x == 1 then a % mod else -a % mod)
  inline def toLong(using inline mod: Int): Long = (rawValue + mod) % mod
  inline def toInt(using inline mod: Int): Int = ((rawValue + mod) % mod).toInt
  override def toString = rawValue.toString

object ModInt:
  inline def zero: ModInt = ModInt(0)
  inline def one: ModInt = ModInt(1)
  inline given Conversion[Int, ModInt] with
    override def apply(x: Int) = ModInt(x)
  extension (value: Long)
    inline def toModInt(using inline mod: Int): ModInt = ModInt(value % mod)
