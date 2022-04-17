package library.string

object SuffixArray {
  private def convertToIntArray(str: String): (Array[Int], Int) =
    val n = str.length
    if n == 0 then return (Array[Int](), 0)
    else if n == 1 then return (Array[Int](0), 1)
    val array = Array.fill(n){0}
    val sorted = str.indices.sortBy(str(_))
    var typeCount = 0
    for i <- 1 until n do
      if str(sorted(i)) != str(sorted(i - 1)) then typeCount += 1
      array(sorted(i)) = typeCount
    (array, typeCount)
  def suffixArray(str: String): Array[Int] =
    val n = str.length
    if n == 0 then return Array[Int]()
    else if n == 1 then return Array[Int](0)
    val (array, typeCount) = convertToIntArray(str)
    suffixArray(array, typeCount)
  private def suffixArray(chars: Array[Int], typeCount: Int): Array[Int] =
    if chars.length < 10 then
      suffixArrayShort(chars)
    else if chars.length < 50 then
      suffixArrayMid(chars)
    else
      suffixArrayLong(chars, typeCount)
  def suffixArrayShort(str: String): Array[Int] =
    suffixArrayShort(convertToIntArray(str)._1)
  private def suffixArrayShort(chars: Array[Int]): Array[Int] =
    val n = chars.length
    val result = chars.indices.toArray
    scala.util.Sorting.stableSort(result, (o1: Int, o2: Int) => {
      var i1 = o1
      var i2 = o2
      while i1 < n && i2 < n && chars(i1) == chars(i2) do
        i1 += 1
        i2 += 1
      if i1 == n then true
      else if i2 == n then false
      else chars(i1).compare(chars(i2)) <= 0
    })
    result
  def suffixArrayMid(str: String): Array[Int] =
    suffixArrayMid(convertToIntArray(str)._1)
  private def suffixArrayMid(chars: Array[Int]): Array[Int] =
    val n = chars.length
    val result = (0 until n).toArray
    var rank = chars.clone()
    var rankTemp = Array.fill(n){0}
    var len = 1
    while len < n do
      scala.util.Sorting.stableSort(result, (o1: Int, o2: Int) => {
        if rank(o1) != rank(o2) then
          rank(o1) < rank(o2)
        else
          val v1 = if o1 + len < n then rank(o1 + len) else -1
          val v2 = if o2 + len < n then rank(o2 + len) else -1
          v1 < v2
      })
      var r = 0
      rankTemp(result(0)) = 0
      for i <- 1 until n do
        val o1 = result(i - 1)
        val o2 = result(i)
        val equal = rank(o1) == rank(o2) && ((o1 + len >= n && o2 + len >= n) || ((o1 + len < n && o2 + len < n) && (rank(o1 + len) == rank(o2 + len))))
        if !equal then
          r += 1
        rankTemp(result(i)) = r
      val temp = rank
      rank = rankTemp
      rankTemp = temp
      len <<= 1
    result
  def suffixArrayLong(str: String): Array[Int] =
    val (array, typeCount) = convertToIntArray(str)
    suffixArrayLong(array, typeCount)
  private def suffixArrayLong(chars: Array[Int], typeCount: Int): Array[Int] =
    val n = chars.length
    val result = Array.fill(n){0}
    val isL = Array.fill(n){false}
    for i <- n - 2 to 0 by -1 do
      isL(i) = if chars(i) == chars(i + 1) then isL(i + 1) else chars(i) < chars(i + 1)
    val sumL = Array.fill(typeCount + 1){0}
    val sumS = Array.fill[Int](typeCount + 1){0}
    for i <- chars.indices do
      if isL(i) then
        sumL(chars(i) + 1) += 1
      else
        sumS(chars(i)) += 1
    for i <- sumL.indices do
      sumS(i) += sumL(i)
      if i < typeCount then
        sumL(i + 1) += sumS(i)
    val lmsMap = Array.fill(n + 1){-1}
    var lmsCount = 0
    for i <- 1 until n do
      if !isL(i - 1) && isL(i) then
        lmsMap(i) = lmsCount
        lmsCount += 1
    val lms = Array.fill(lmsCount){0}
    for i <- 1 until n do
      if lmsMap(i) >= 0 then
        lms(lmsMap(i)) = i
    val buffer = Array.fill(n){0}
    def induceSort(lms: Array[Int]): Unit =
      java.util.Arrays.fill(result, -1)
      sumS.copyToArray(buffer)
      for s <- lms do
        result(buffer(chars(s))) = s
        buffer(chars(s)) += 1
      sumL.copyToArray(buffer)
      result(buffer(chars(n - 1))) = n - 1
      buffer(chars(n - 1)) += 1
      for v <- result do
        if v >= 1 && !isL(v - 1) then
          result(buffer(chars(v - 1))) = v - 1
          buffer(chars(v - 1)) += 1
      sumL.copyToArray(buffer)
      for i <- result.indices.reverse do
        val v = result(i)
        if v >= 1 && isL(v - 1) then
          buffer(chars(v - 1) + 1) -= 1
          result(buffer(chars(v - 1) + 1)) = v - 1
    induceSort(lms)
    if lmsCount > 0 then
      val sorted = Array.fill(lmsCount){0}
      var idx = 0
      for v <- result do
        if lmsMap(v) != -1 then
          sorted(idx) = v
          idx += 1
      val lmsSubstring = Array.fill(lmsCount){0}
      var recTypeCount = 0
      lmsSubstring(lmsMap(sorted(0))) = 0
      for i <- 1 until lmsCount do
        var l = sorted(i - 1)
        var r = sorted(i)
        val endL = if lmsMap(l) + 1 < lmsCount then lms(lmsMap(l) + 1) else n
        val endR = if lmsMap(r) + 1 < lmsCount then lms(lmsMap(r) + 1) else n
        var same = true
        if endL - l != endR - r then
          same = false
        else
          while l < endL && chars(l) == chars(r) do
            l += 1
            r += 1
          if l == n || chars(l) != chars(r) then
            same = false
        if !same then
          recTypeCount += 1
        lmsSubstring(lmsMap(sorted(i))) = recTypeCount
      val sa = suffixArray(lmsSubstring, recTypeCount)
      for i <- 0 until lmsCount do
        sorted(i) = lms(sa(i))
      induceSort(sorted)
    result
  def lcpArray(str: String, suffixArray: Array[Int]): Array[Int] =
    val n = str.length
    if n <= 1 then return Array()
    val length = Array.fill(n){0}
    for i <- 0 until n - 1 do
      length(suffixArray(i)) = suffixArray(i + 1)
    length(suffixArray.last) = n
    var len = 0
    for i <- 0 until n do
      if (len > 0) len -= 1
      val j = length(i)
      while i + len < n && j + len < n && str(i + len) == str(j + len) do
        len += 1
      length(i) = len
    Array.tabulate(n - 1){i => length(suffixArray(i))}
      
}
