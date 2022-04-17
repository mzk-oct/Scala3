package library.flow

import scala.collection.mutable

object PushRelabel2:
  def pushFlow(source: Int, sink: Int, network: FlowNetworkList): Int =
    val n = network.size
    val capacity = Array.fill(n){Array.fill(n){0}}
    for i <- 0 until n do
      for e <- network.graph(i) do
        capacity(i)(e.to) += e.capacity
    val edges = Array.tabulate(n){i => (0 until n).filter{j => capacity(i)(j) != 0 || capacity(j)(i) != 0}.toArray}
    val indices = Array.fill(n){0}
    val heightStack = Array.fill(2 * n){Array.fill(n - 2){0}}
    val heightIndices = Array.fill(2 * n){0}
    val heights = Array.fill(n){0}
    heights(source) = n
    var maxHeight = -1
    val preFlow = Array.fill(n){0}
    for to <- edges(source) do
      if capacity(source)(to) > 0 then
        preFlow(to) += capacity(source)(to)
        capacity(to)(source) += capacity(source)(to)
        capacity(source)(to) = 0
        if to != sink then
          heightStack(0)(heightIndices(0)) = to
          heightIndices(0) += 1
          maxHeight = 0
    while maxHeight >= 0 do
      val node = heightStack(maxHeight)(heightIndices(maxHeight) - 1)
      heightIndices(maxHeight) -= 1
      var flow = preFlow(node)
      var index = indices(node)
      var height = heights(node)
      while flow > 0 do
        if index == edges(node).length then
          var nextHeight = Int.MaxValue
          for to <- edges(node) do
            if capacity(node)(to) > 0 then
              nextHeight = scala.math.min(nextHeight, heights(to))
          nextHeight += 1
          height = nextHeight
          index = 0
        else
          val to = edges(node)(index)
          if heights(to) + 1 == height && capacity(node)(to) > 0 then
            val f = scala.math.min(flow, capacity(node)(to))
            flow -= f
            capacity(node)(to) -= f
            capacity(to)(node) += f
            if preFlow(to) == 0 && to != source && to != sink then
              heightStack(height - 1)(heightIndices(height - 1)) = to
              heightIndices(height - 1) += 1
              maxHeight = scala.math.max(maxHeight, height - 1)
            preFlow(to) += f
            if capacity(node)(to) == 0 then
              index += 1
          else
            index += 1
      preFlow(node) = 0
      indices(node) = index
      heights(node) = height
      while maxHeight >= 0 && heightIndices(maxHeight) == 0 do
        maxHeight -= 1
    preFlow(sink)