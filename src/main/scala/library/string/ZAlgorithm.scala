package library.string

object ZAlgorithm:
  def longestCommonPrefixes(str: String): Array[Int] =
    val n = str.length
    if n == 0 then return Array()
    if n == 1 then return Array(1)
    val result = Array.fill(n){0}
    var left = 1
    var right = 1
    for i <- 1 until n do
      right = scala.math.max(right, i)
      if result(i - left) + i >= right then
        left = i
        while right < n && str(right - left) == str(right) do
          right += 1
        result(i) = right - i
      else
        result(i) = result(i - left)
    result(0) = n
    result
