package library.tree
import scala.annotation.tailrec

object Patricia:
  private sealed trait Node extends Iterable[Int]:
    val prefix: Int
    override def iterator = NodeIterator(this)
  private object Node:
    private def leftMostOne(value: Int): Int =
      var result = 0
      for i <- 4 to 0 by -1 do
        if value >>> (result + (1 << i)) > 0 then
          result += 1 << i
      1 << result
    private def apply(left: Node, right: Node): Node =
      val mask = -leftMostOne(left.prefix ^ right.prefix)
      val prefix = (left.prefix | right.prefix) & mask
      Branch(prefix, left, right)
    def check(node: Node): Unit =
      node match
        case Branch(prefix, left, right) =>
          val mask = -(prefix & -prefix) << 1
          assert(left.prefix < right.prefix)
          assert((prefix & mask) == (left.prefix & mask))
          assert((prefix & mask) == (right.prefix & mask))
          assert((left.prefix & prefix) != (right.prefix & prefix))
          check(left)
          check(right)
        case _ =>
    def insert(node: Node, newValue: Int): Node =
      node match
        case Branch(prefix, left, right) =>
          val mask = -(prefix & -prefix) << 1
          if (prefix & mask) != (newValue & mask) then
            if prefix < newValue then Node(node, Leaf(newValue))
            else Node(Leaf(newValue), node)
          else
            if (newValue & prefix) == (left.prefix & prefix) then
              val newLeft = insert(left, newValue)
              if newLeft eq left then node
              else Branch(prefix, newLeft, right)
            else
              val newRight = insert(right, newValue)
              if newRight eq right then node
              else Branch(prefix, left, newRight)
        case Leaf(prefix) =>
          if prefix == newValue then node
          else if prefix < newValue then Node(node, Leaf(newValue))
          else Node(Leaf(newValue), node)
    def erase(node: Node, value: Int): Option[Node] =
      node match
        case Branch(prefix, left, right) =>
          val mask = -(prefix & -prefix) << 1
          if (prefix & mask) != (value & mask) then Some(node)
          else
            if (value & prefix) == (left.prefix & prefix) then
              erase(left, value) match
                case Some(newLeft) =>
                  if newLeft eq left then Some(node)
                  else Some(Node(newLeft, right))
                case None  => Some(right)
            else
              erase(right, value) match
                case Some(newRight) =>
                  if newRight eq right then Some(node)
                  else Some(Node(left, newRight))
                case None => Some(left)
        case Leaf(oldValue) =>
          if oldValue == value then None
          else Some(node)
    @tailrec def minValue(node: Node): Int =
      node match
        case Branch(_, left, _) => minValue(left)
        case Leaf(prefix) => prefix
    @tailrec def maxValue(node: Node): Int =
      node match
        case Branch(_, _, right) => maxValue(right)
        case Leaf(prefix) => prefix
    @tailrec def search(node: Node, value: Int): Boolean =
      node match
        case Branch(prefix, left, right) =>
          val mask = -(prefix & -prefix) << 1
          if (prefix & mask) != (value & mask) then false
          else if (value & prefix) == (left.prefix & prefix) then search(left, value)
          else search(right, value)
        case Leaf(prefix) => prefix == value
    def greaterThanOrEqual(node: Node, value: Int): Option[Int] =
      node match
        case Branch(prefix, left , right) =>
          val mask = -(prefix & -prefix) << 1
          if (prefix & mask) < (value & mask) then None
          else if (value & mask) < (prefix & mask) then Some(minValue(left))
          else if (value & prefix) == (right.prefix & prefix) then greaterThanOrEqual(right, value)
          else Some(greaterThanOrEqual(left, value).getOrElse(minValue(right)))
        case Leaf(prefix) =>
          if value <= prefix then Some(prefix)
          else None
    def greaterThan(node: Node, value: Int): Option[Int] =
      node match
        case Branch(prefix, left , right) =>
          val mask = -(prefix & -prefix) << 1
          if (prefix & mask) < (value & mask) then None
          else if (value & mask) < (prefix & mask) then Some(minValue(left))
          else if (value & prefix) == (right.prefix & prefix) then greaterThan(right, value)
          else Some(greaterThan(left, value).getOrElse(minValue(right)))
        case Leaf(prefix) =>
          if value < prefix then Some(prefix)
          else None
    def leesThanOrEqual(node: Node, value: Int): Option[Int] =
      node match
        case Branch(prefix, left, right) =>
          val mask = -(prefix & -prefix) << 1
          if (value & mask) < (prefix & mask) then None
          else if (prefix & mask) < (value & mask) then Some(maxValue(right))
          else if (value & prefix) == (right.prefix & prefix) then Some(leesThanOrEqual(right, value).getOrElse(maxValue(left)))
          else leesThanOrEqual(left, value)
        case Leaf(prefix) =>
          if prefix <= value then Some(prefix)
          else None
    def lessThan(node: Node, value: Int): Option[Int] =
      node match
        case Branch(prefix, left, right) =>
          val mask = -(prefix & -prefix) << 1
          if (value & mask) < (prefix & mask) then None
          else if (prefix & mask) < (value & mask) then Some(maxValue(right))
          else if (value & prefix) == (right.prefix & prefix) then Some(lessThan(right, value).getOrElse(maxValue(left)))
          else lessThan(left, value)
        case Leaf(prefix) =>
          if prefix < value then Some(prefix)
          else None
    def copyToArray(node: Node, dest: Array[Int], position: Int): Int =
      node match
        case Branch(prefix, left, right) =>
          copyToArray(right, dest, copyToArray(left, dest, position))
        case Leaf(prefix) =>
          dest(position) = prefix
          position + 1
  private case class Branch(override val prefix: Int, val left: Node, val right: Node) extends Node
  private case class Leaf(override val prefix: Int) extends Node
  private class NodeIterator(node: Node) extends Iterator[Int]:
    private val stack = Array.fill(32){node}
    private var index = 0
    private def untilLeaf(): Unit =
      if index >= 0 then
        stack(index) match
          case Branch(_, left, right) =>
            stack(index) = right
            stack(index + 1) = left
            index += 1
            untilLeaf()
          case Leaf(value) =>
    untilLeaf()
    override def hasNext = index >= 0
    override def next(): Int =
      val current = stack(index).prefix
      index -= 1
      untilLeaf()
      current

class Patricia private(private val root: Option[Patricia.Node], override val size: Int) extends Iterable[Int]:
  import Patricia.Node.*
  import Patricia.*
  def this() =
    this(None, 0)
  override def iterator: Iterator[Int] = root.map(_.iterator).getOrElse(Iterator.empty)
  def contains(value: Int): Boolean = root.exists(Node.search(_, value))
  def inserted(value: Int): Patricia =
    root match
      case Some(r) =>
        val newRoot = insert(r, value)
        if newRoot eq r then this
        else Patricia(Some(newRoot), size + 1)
      case None => Patricia(Some(Patricia.Leaf(value)), 1)
  def erased(value: Int): Patricia =
    root match
      case Some(r) =>
        erase(r, value) match
          case Some(newRoot) =>
            if newRoot eq r then this
            else Patricia(Some(newRoot), size - 1)
          case None => Patricia(None, 0)
      case None => this
  def toArray: Array[Int] =
    root match
      case Some(r) =>
        val result = Array.fill(size){0}
        Node.copyToArray(r, result, 0)
        result
      case None => Array()
  def greaterThan(value: Int): Option[Int] = root.flatMap(Node.greaterThan(_, value))
  def greaterThanOrEqual(value: Int): Option[Int] = root.flatMap(Node.greaterThanOrEqual(_, value))
  def lessThan(value: Int): Option[Int] = root.flatMap(Node.lessThan(_, value))
  def lessThanOrEqual(value: Int): Option[Int] = root.flatMap(Node.leesThanOrEqual(_, value))