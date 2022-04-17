package library.math

import scala.annotation.tailrec
class Rational private (private var num: Long, private var den: Long):
  def numerator: Long = num
  def denominator: Long = den
  def +(other: Rational): Rational = Rational.from(num * other.den + den * other.num, den * other.den)
  def -(other: Rational): Rational = Rational.from(num * other.den - den * other.num, den * other.den)
  def *(other: Rational): Rational = Rational.from(num * other.num, den * other.den)
  def /(other: Rational): Rational = Rational.from(num * other.den, den * other.num)
  def +=(other: Rational) =
    val (n, d) = Rational.normalize(num * other.den + den * other.num, den * other.den)
    num = n
    den = d
  def -=(other: Rational) =
    val (n, d) = Rational.normalize(num * other.den - den * other.num, den * other.den)
    num = n
    den = d
  def *=(other: Rational) =
    val (n, d) = Rational.normalize(num * other.num, den * other.den)
    num = n
    den = d
  def /=(other: Rational) =
    val (n, d) = Rational.normalize(num * other.den, den * other.num)
    num = n
    den = d
    
object Rational:
  private inline def normalize(num: Long, den: Long): (Long, Long) =
    val g = gcd(num, den)
    val sign = num.sign * den.sign
    (num.abs * sign / g, den.abs / g)

  def from(num: Long, den: Long): Rational =
    val (n, d) = normalize(num, den)
    Rational(n, d)
  @tailrec
  def gcd(a: Long, b: Long): Long =
    b match
      case 0 => a
      case _ => gcd(b, a % b)