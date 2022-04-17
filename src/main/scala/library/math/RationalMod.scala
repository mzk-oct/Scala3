package library.math

import ModIntOpaque.*
class RationalMod(private var num: ModInt, private var den: ModInt):
  inline def numerator: ModInt = num
  inline def denominator: ModInt = den
  inline def +(other: RationalMod)(using inline mod: Int): RationalMod = RationalMod(num * other.den + den * other.num, den * other.den)
  inline def -(other: RationalMod)(using inline mod: Int): RationalMod = RationalMod(num * other.den - den * other.num, den * other.den)
  inline def *(other: RationalMod)(using inline mod: Int): RationalMod = RationalMod(num * other.num, den * other.den)
  inline def /(other: RationalMod)(using inline mod: Int): RationalMod = RationalMod(num * other.den, den * other.num)
  inline def toLong(using inline mod: Int): Long = (num * den.inverse).toLong