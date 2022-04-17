package library.flow

import scala.collection.mutable

object Dinic:
  import scala.math.min
  def pushFlow(source: Int, sink: Int, network: FlowNetworkList): Int =
    if (network.graph.length < 2) throw IllegalThreadStateException()
    val graph = network.graph
    val deque = Array.fill(graph.length){0}
    val depth = Array.fill(graph.length){0}
    val prevEdge = Array.fill(graph.length){Edge(-1, -1, 0)}
    val indices = Array.fill(graph.length){0}
    var flow = 0
    var hasFlow = true
    inline def validEdge(from: Int, edge: Edge): Boolean =
      edge.capacity > 0 &&
        depth(from) < depth(edge.to) &&
        indices(edge.to) < graph(edge.to).size
    while hasFlow do
      hasFlow = false
      java.util.Arrays.fill(depth, -1)
      depth(source) = 0
      var begin = 0
      var end = 1
      deque(0) = source
      while begin < end && depth(sink) == -1 do
        val current = deque(begin)
        begin += 1
        for e <- graph(current) do
          if e.capacity > 0 && depth(e.to) == -1 then
            depth(e.to) = depth(current) + 1
            deque(end) = e.to
            end += 1
      java.util.Arrays.fill(indices, 0)
      end = if depth(sink) == -1 then 0 else 1
      deque(0) = source
      while end != 0 do
        while end != 0 && deque(end - 1) != sink do
          val current = deque(end - 1)
          var index = indices(current)
          while index < graph(current).size && !validEdge(current, graph(current)(index)) do
            index += 1
          indices(current) = index
          if index < graph(current).size then
            val edge = graph(current)(index)
            prevEdge(edge.to) = edge
            deque(end) = edge.to
            end += 1
          else
            end -= 1
        if end != 0 then
          var minFlow = Int.MaxValue
          for i <- 1 until end do
            minFlow = min(minFlow, prevEdge(deque(i)).capacity)
          flow += minFlow
          hasFlow = true
          for i <- end - 1 to 1 by -1 do
            if prevEdge(deque(i)).capacity == minFlow then
              end = i
            val edge = prevEdge(deque(i))
            val reverseEdge = graph(edge.to)(edge.pair)
            edge.capacity -= minFlow
            reverseEdge.capacity += minFlow
    flow
  
  def pushFlow(source: Int, sink: Int, network: FlowNetworkMatrix): Int =
    if (network.graph.length < 2) throw IllegalThreadStateException()
    val n = network.size
    val capacity = network.graph
    val deque = Array.fill(n){0}
    val depth = Array.fill(n){0}
    val indices = Array.fill(n){0}
    var flow = 0
    var hasFlow = true
    inline def validEdge(from: Int, to: Int): Boolean =
      capacity(from)(to) > 0 &&
        depth(from) < depth(to) &&
        indices(to) < n
    while hasFlow do
      hasFlow = false
      java.util.Arrays.fill(depth, -1)
      depth(source) = 0
      var begin = 0
      var end = 1
      deque(0) = source
      while begin < end && depth(sink) == -1 do
        val current = deque(begin)
        begin += 1
        for to <- 0 until n do
          if capacity(current)(to) > 0 && depth(to) == -1 then
            depth(to) = depth(current) + 1
            deque(end) = to
            end += 1
      java.util.Arrays.fill(indices, 0)
      end = if depth(sink) == -1 then 0 else 1
      deque(0) = source
      while end != 0 do
        while end != 0 && deque(end - 1) != sink do
          val current = deque(end - 1)
          var index = indices(current)
          while index < n && !validEdge(current, index) do
            index += 1
          indices(current) = index
          if index < n then
            val to = index
            deque(end) = to
            end += 1
          else
            end -= 1
        if end != 0 then
          var minFlow = Int.MaxValue
          for i <- 1 until end do
            minFlow = min(minFlow, capacity(deque(i - 1))(deque(i)))
          flow += minFlow
          hasFlow = true
          for i <- end - 1 to 1 by -1 do
            val from = deque(i - 1)
            val to = deque(i)
            if capacity(from)(to) == minFlow then
              end = i
            capacity(from)(to) -= minFlow
            capacity(to)(from) += minFlow
    flow
