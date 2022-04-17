package library.flow

object FordFulkerson:
  import scala.math.*
  def pushFlow(source: Int, sink: Int, flowNetwork: FlowNetworkList): Int =
    val graph = flowNetwork.graph
    val visit = Array.fill(graph.size){false}
    def dfs(current: Int, minFlow: Int = Int.MaxValue): Int =
      if current == sink then return minFlow
      visit(current) = true
      for edge <- graph(current) do
        if edge.capacity > 0 && !visit(edge.to) then
          val flow = dfs(edge.to, min(edge.capacity, minFlow))
          if flow > 0 then
            edge.capacity -= flow
            graph(edge.to)(edge.pair).capacity += flow
            return flow
      0
    var flow = 0
    var hasFlow = true
    while hasFlow do
      java.util.Arrays.fill(visit, false)
      val f = dfs(source)
      if f > 0 then
        flow += f
      else 
        hasFlow = false
    flow



