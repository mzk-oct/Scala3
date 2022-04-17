package library.math


object ModIntOpaque:
  opaque type ModInt = Long
  inline def zero: ModInt = 0
  inline def one: ModInt = 1
  extension (value: Long)
    inline def toModInt(using inline mod: Int): ModInt =  value % mod
    inline def unsafeAsModInt: ModInt = value
  extension (value: Int)
    inline def toModInt(using inline mod: Int): ModInt = value % mod
    inline def unsafeAsModInt: ModInt = value
  trait Converter[T]:
    inline def convert(value: T)(using inline mod: Int): ModInt
  inline given Converter[Int] with
    override inline def convert(value: Int)(using inline mod: Int) = value.toModInt
  inline given Converter[Long] with
    override inline def convert(value: Long)(using inline mod: Int) = value.toModInt
  inline given Converter[ModInt] with
    override inline def convert(value: ModInt)(using inline mod: Int) = value
  extension (modInt: ModInt)
    inline def value(using inline mod: Int): Long = (modInt + mod) % mod
    inline def rawValue: Long = modInt
    inline def inverse(using inline mod: Int): ModInt =
      var x = modInt
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
        y = c * modInt + d * mod
      require(x.abs == 1L)
      if x == 1 then a % mod else -a % mod
    inline def pow(exp: Long)(using inline mod: Int): ModInt =
      var result = 1L
      var base = modInt
      var e = exp
      while e > 0 do
        if (e & 1) == 1 then
          result = result * base % mod
        base = base * base % mod
        e >>= 1
      result
    inline def pow(exp: Int)(using inline mod: Int): ModInt = pow(exp.toLong)
    inline def toLong(using inline mod: Int): Long = (modInt + mod) % mod
  inline def powMod[B: Converter] (base: B, exp: Int)(using inline mod: Int): ModInt = summon[Converter[B]].convert(base).pow(exp)
  inline def powMod[B: Converter] (base: B, exp: Long)(using inline mod: Int): ModInt = summon[Converter[B]].convert(base).pow(exp)
  extension [L: Converter, R: Converter] (left: L)
    inline def +(right: R)(using inline mod: Int): ModInt = (summon[Converter[L]].convert(left) + summon[Converter[R]].convert(right)).toModInt
    inline def -(right: R)(using inline mod: Int): ModInt = (summon[Converter[L]].convert(left) - summon[Converter[R]].convert(right)).toModInt
    inline def *(right: R)(using inline mod: Int): ModInt = (summon[Converter[L]].convert(left) * summon[Converter[R]].convert(right)).toModInt
    inline def /(right: R)(using inline mod: Int): ModInt = (summon[Converter[L]].convert(left) * summon[Converter[R]].convert(right).inverse).toModInt


