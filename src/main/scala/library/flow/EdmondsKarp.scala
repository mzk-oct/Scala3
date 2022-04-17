package library.flow

import scala.collection.mutable
import scala.math.*
object EdmondsKarp:
  def pushFlow(source: Int, sink: Int, network: FlowNetworkList): Int =
    val graph = network.graph
    val queue = mutable.Queue[Int]()
    val prevEdge = Array.fill(graph.length){Edge(-1, -1, 0)}
    var result = 0
    val visit = Array.fill(graph.length){false}
    var hasFlow = true
    while hasFlow do
      java.util.Arrays.fill(visit, false)
      visit(source) = true
      queue.clear()
      queue.append(source)
      hasFlow = false
      while queue.nonEmpty && !visit(sink) do
        val current = queue.dequeue()
        for e <- graph(current) do
          if e.capacity > 0 && !visit(e.to) then
            visit(e.to) = true
            queue.append(e.to)
            prevEdge(e.to) = e
      if visit(sink) then
        hasFlow = true
        var last = sink
        var minCapacity = Int.MaxValue
        while last != source do
          val edge = prevEdge(last)
          minCapacity = min(minCapacity, edge.capacity)
          last = graph(edge.to)(edge.pair).to
        result += minCapacity
        last = sink
        while last != source do
          val edge = prevEdge(last)
          val pairEdge = graph(edge.to)(edge.pair)
          edge.capacity -= minCapacity
          pairEdge.capacity += minCapacity
          last = pairEdge.to
    result

