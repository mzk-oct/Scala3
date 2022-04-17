package library.tree

class UnionFind(val size: Int):
  private val vec = Array.fill(size){-1}
  def find(node: Int): Int =
    if vec(node) < 0 then
      node
    else
      vec(node) = find(vec(node))
      vec(node)
  def same(a: Int, b: Int): Boolean = find(a) == find(b)
  def unite(a: Int, b: Int): Unit =
    val rootA = find(a)
    val rootB = find(b)
    if rootA == rootB then return
    if vec(rootA) <= vec(rootB) then
      vec(rootA) += vec(rootB)
      vec(rootB) = rootA
    else
      vec(rootB) += vec(rootA)
      vec(rootA) = rootB
  def sizeOf(node: Int): Int = -vec(find(node))
