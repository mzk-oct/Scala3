package library.math
import ModIntOpaque.*
class MatrixModInt private(val row: Int, val column: Int, private val matrix: Array[Array[ModInt]]):
  import library.math.MatrixModInt.inPlaceTimes
  def this(row: Int, column: Int) =
    this(row, column, Array.fill(row){Array.fill(column){ModIntOpaque.zero}})
  def apply(i: Int, j: Int): ModInt =
    matrix(i)(j)
  def update(i: Int, j: Int, value: ModInt) =
    matrix(i)(j) = value
  inline def *(right: MatrixModInt)(using inline mod: Int): MatrixModInt =
    val newRow = row
    val newColumn = right.column
    val mid = column
    MatrixModInt.tabulate(newRow, newColumn){(i, j) =>
      (0 until mid).foldLeft(ModIntOpaque.zero){ (acc, k) => acc + matrix(i)(k) * right.matrix(k)(j) }
    }
  inline def pow(exp: Long)(using inline mod: Int): MatrixModInt =
    require(row == column)
    var temp = MatrixModInt(row, row)
    var result = MatrixModInt.e(row, row)
    var base = this
    var e = exp
    while e > 0 do
      if (e & 1) == 1 then
        inPlaceTimes(result, base, temp)
        val t = temp
        temp = result
        result = t
      inPlaceTimes(base, base, temp)
      val t = temp
      temp = base
      base = t
      e >>= 1
    result

object MatrixModInt:
  def tabulate(row: Int, column: Int)(generator: (Int, Int) => ModInt): MatrixModInt =
    MatrixModInt(row, column, Array.tabulate(row){i => Array.tabulate(column){j => generator(i, j)}})
  def e(row: Int, column: Int): MatrixModInt =
    MatrixModInt.tabulate(row, column){(i, j) =>
      if i == j then ModIntOpaque.one else ModIntOpaque.zero
    }
  inline def inPlaceTimes(left: MatrixModInt, right: MatrixModInt, dest: MatrixModInt)(using inline mod: Int) =
    val newRow = left.row
    val newColumn = right.column
    val mid = left.column
    for
      i <- 0 until newRow
      j <- 0 until newColumn
    do
      var sum = ModIntOpaque.zero
      for k <- 0 until mid do
        sum = sum + left(i, k) * right(k, j)
      dest(i, j) = sum

