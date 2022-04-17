package library.flow

class FlowNetworkMatrix private (val size: Int, val graph: Array[Array[Int]]):
  def this(size: Int) =
    this(size, Array.fill(size){Array.fill(size){0}})
  def addEdge(from: Int, to: Int, capacity: Int) =
    graph(from)(to) += capacity

