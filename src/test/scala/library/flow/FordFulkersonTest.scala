package library.flow

import org.scalatest.funsuite.AnyFunSuite

import scala.util.Random

class FordFulkersonTest extends AnyFunSuite {

  val random = Random(2)
  for iteration <- 0 until 10000 do
    val size = random.nextInt(200) + 2
    val density = random.nextDouble()
    val seed = random.nextInt()
    val (network, networkM) = makeNetwork(size, density, seed)
    val dinic2 = Dinic.pushFlow(0, size - 1, network)
    val dinic = Dinic.pushFlow(0, size - 1, networkM)
    test(s"test case: $iteration, size: $size, edgeCount: ${network.graph.map{_.size}.sum}, flow: $dinic") {
      assert(dinic == dinic2)
    }



  def makeNetwork(size: Int, density: Double, seed: Int): (FlowNetworkList, FlowNetworkMatrix) =
    val random = Random(seed)
    val result = FlowNetworkList(size)
    val resultM = FlowNetworkMatrix(size)
    for i <- 0 until size do
      for j <- 0 until size do
        if i != j && random.nextDouble() <= density then
          val capacity = random.nextInt(10) + 1
          result.addEdge(i, j, capacity)
          resultM.addEdge(i, j, capacity)
    (result, resultM)
}
