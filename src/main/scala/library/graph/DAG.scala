package library.graph

import scala.collection.mutable.ArrayBuffer

object DAG:
  def dag(graph: Array[ArrayBuffer[Int]]): Array[Int] =
    val n = graph.length
    val indices = Array.fill(n){0}
    val stack = Array.fill(n){0}
    val size = Array.fill(n){0}
    var pos = 0
    var outPos = n - 1
    for i <- 0 until n do
      if indices(i) == 0 then
        stack(pos) = i
        pos += 1
        while pos > 0 do
          pos -= 1
          val top = stack(pos)
          val edge = graph(top)
          if indices(top) == edge.size then
            indices(top) = -1
            stack(outPos) = top
            outPos -= 1
          else
            var index = indices(top)
            while index < edge.length && indices(edge(index)) != 0 do
              size(edge(index)) += 1
              index += 1
            pos += 1
            if index < edge.size then
              stack(pos) = edge(index)
              pos += 1
              size(edge(index)) += 1
              index += 1
            indices(top) = index
    val reverseGraph = Array.tabulate(n){i => Array.fill(size(i)){0}}
    for from <- graph.indices do
      for to <- graph(from) do
        size(to) -= 1
        reverseGraph(to)(size(to)) = from
    val newStack = size
    val result = indices
    for i <- stack do
      if result(i) < 0 then
        outPos += 1
        result(i) = outPos
        newStack(pos) = i
        pos += 1
        while pos > 0 do
          pos -= 1
          val from = newStack(pos)
          for to <- reverseGraph(from) do
            if result(to) < 0 then
              result(to) = outPos
              size(pos) = to
              pos += 1
    result