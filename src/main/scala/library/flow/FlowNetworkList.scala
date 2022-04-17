package library.flow

import scala.collection.mutable.ArrayBuffer


class Edge(val to: Int, val pair: Int, var capacity: Int)
class FlowNetworkList private(val size: Int, val graph: Array[ArrayBuffer[Edge]]) {
  def this(size: Int) =
    this(size, Array.fill(size){ArrayBuffer()})
  def addEdge(from: Int, to: Int, capacity: Int) =
    graph(from).append(Edge(to, graph(to).length, capacity))
    graph(to).append(Edge(from, graph(from).length - 1, 0))

  override def clone(): FlowNetworkList =
    FlowNetworkList(size, graph.map{ buffer => buffer.map{ e => Edge(e.to, e.pair, e.capacity)}})
}
