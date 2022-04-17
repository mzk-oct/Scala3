package library.math

object Permutation:
  extension[T] (array: Array[T])
    inline def nextPermutation()(using ord: Ordering[T]): Boolean =
      (0 until array.length - 1).findLast(i => ord.compare(array(i), array(i + 1)) < 0).flatMap { last =>
        (last + 1 until array.length).findLast(i => ord.compare(array(last), array(i)) < 0).map((last, _))
      } match
        case Some((i, j)) =>
          val head = array(j)
          array(j) = array(i)
          array(i) = head
          var l = i + 1
          var r = array.length - 1
          while l < r do
            val temp = array(l)
            array(l) = array(r)
            array(r) = temp
            l += 1
            r -= 1
          true
        case None =>
          false
