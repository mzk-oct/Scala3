package library.flow

import scala.math.hypot

object PushRelabel:
  import scala.math.min
  def pushFlow(source: Int, sink: Int, network: FlowNetworkList): Int =
    if network.graph.length == 2 then
      network.graph(source).map(e => if e.to == sink then e.capacity else 0).sum
    else
      pushFlowInner(source, sink, network)
  private[this] def pushFlowInner(source: Int, sink: Int, network: FlowNetworkList): Int =
    val graph = network.graph
    val height = Array.fill(graph.length){0}
    height(source) = graph.length
    val preFlow = Array.fill(graph.length){0}
    for e <- graph(source) do
      preFlow(e.to) += e.capacity
      graph(e.to)(e.pair).capacity += e.capacity
      e.capacity = 0
    inline def newHeight(node: Int): Int =
      var minHeight = Int.MaxValue
      for e <- graph(node) do
        if e.capacity > 0 then
          minHeight = min(minHeight, height(e.to))
      minHeight + 1
    val indices = Array.fill(graph.length){0}
    inline def push(node: Int): Unit =
      var flow = preFlow(node)
      var index = indices(node)
      val edges = graph(node)
      var h = height(node)
      while flow > 0 do
        if index == edges.length then
          h = newHeight(node)
          index = 0
        else
          val e = edges(index)
          if e.capacity > 0 && h == height(e.to) + 1 then
            val f = min(flow, e.capacity)
            e.capacity -= f
            graph(e.to)(e.pair).capacity += f
            preFlow(e.to) += f
            flow -= f
          else
            index += 1
      height(node) = h
      indices(node) = index
      preFlow(node) = 0
    val next = Array.tabulate(graph.length){i => (i + 1) % graph.length}
    val prev = Array.tabulate(graph.length){i => (i - 1 + graph.length) % graph.length}
    inline def remove(node: Int): Unit =
      next(prev(node)) = next(node)
      prev(next(node)) = prev(node)
    inline def addNext(prevNode: Int, node: Int): Unit =
      next(node) = next(prevNode)
      prev(node) = prevNode
      prev(next(prevNode)) = node
      next(prevNode) = node
    remove(source)
    remove(sink)
    addNext(graph.indices.find(i => i != source && i != sink).get, source)
    var current = next(source)
    while current != source do
      val oldHeight = height(current)
      push(current)
      if height(current) > oldHeight then
        remove(current)
        addNext(source, current)
      current = next(current)
    preFlow(sink)

