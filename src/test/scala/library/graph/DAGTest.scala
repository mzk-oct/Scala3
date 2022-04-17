package library.graph

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class DAGTest extends AnyFunSuite, Matchers {
  val random = Random(0)

  for iteration <- 1 to 3000 do
    val seed = random.nextInt()
    val size = random.nextInt(300)
    val prob = random.nextDouble() * 0.1
    val graph = makeGraph(seed, size, prob)
    val group = DAG.dag(graph)
    val reachable = search(graph)
    val groupCount = group.distinct.size
    val edgeCount = graph.map(_.length).sum
    test(s"test case: $iteration, size: $size, prob: $prob, groupCount: $groupCount, edgeCount: $edgeCount") {
      assert((0 until size).forall{i => (0 until size).forall{j => i == j || ((group(i) == group(j)) == (reachable(i)(j) && reachable(j)(i)))}}, "\n" ++ group.zipWithIndex.mkString(",") ++ "\n:" ++ graph.map(_.mkString(", ")).mkString("\n"))
    }

  def makeGraph(seed: Int, size: Int, edgeProb: Double): Array[ArrayBuffer[Int]] =
    val random = Random(seed)
    val result = Array.fill(size){ArrayBuffer[Int]()}
    for from <- 0 until size do
      for to <- 0 until size do
        if from != to && random.nextDouble() <= edgeProb then
          result(from).append(to)
    result

  def search(graph: Array[ArrayBuffer[Int]]): Array[Array[Boolean]] =
    val n = graph.length
    val result = Array.fill(n){Array.fill(n){false}}
    for from <- graph.indices do
      for to <- graph(from) do
        result(from)(to) = true
    for k <- 0 until n do
      for i <- 0 until n do
        for j <- 0 until n do
          result(i)(j) ||= result(i)(k) && result(k)(j)
    result
}
