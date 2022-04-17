package library.math
import library.category.SemiRing

import scala.reflect.{ClassTag, ensureAccessible}

class Matrix[T: SemiRing: ClassTag] private(val row: Int, val column: Int, private val matrix: Array[Array[T]]):
  def this(row: Int, column: Int) =
    this(row, column, Array.fill(row){Array.fill(column){summon[SemiRing[T]].zero}})
  def apply(i: Int, j: Int): T =
    matrix(i)(j)
  def update(i: Int, j: Int, value: T) =
    matrix(i)(j) = value
  def *(right: Matrix[T]): Matrix[T] =
    val newRow = row
    val newColumn = right.column
    val mid = column
    Matrix.tabulate(newRow, newColumn){(i, j) =>
      (0 until mid).foldLeft(summon[SemiRing[T]].zero){(acc, k) => summon[SemiRing[T]].plus(acc, summon[SemiRing[T]].times(matrix(i)(k), right.matrix(k)(j))) }
    }
  def pow(exp: Long): Matrix[T] =
    require(row == column)
    var result = Matrix.e(row, row)
    var base = this
    var e = exp
    while e > 0 do
      if (e & 1) == 1 then
        result *= base
      base *= base
      e >>= 1
    result

object Matrix:
  def tabulate[T: SemiRing: ClassTag](row: Int, column: Int)(generator: (Int, Int) => T): Matrix[T] =
    Matrix(row, column, Array.tabulate(row){i => Array.tabulate(column){j => generator(i, j)}})
  def e[T: SemiRing: ClassTag](row: Int, column: Int): Matrix[T] =
    Matrix.tabulate(row, column){(i, j) =>
      if i == j then summon[SemiRing[T]].one else summon[SemiRing[T]].zero
    }
  private def inPlaceTimes[T: SemiRing: ClassTag](left: Matrix[T], right: Matrix[T], dest: Matrix[T]) =
    val newRow = left.row
    val newColumn = right.column
    val mid = left.column
    for
      i <- 0 until newRow
      j <- 0 until newColumn
    do
      var sum = summon[SemiRing[T]].zero
      for k <- 0 until mid do
        sum = summon[SemiRing[T]].plus(sum, summon[SemiRing[T]].times(left(i, k), right(k, j)))
      dest(i, j) = sum

