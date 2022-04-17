package library.tree

import library.category.Monoid
import library.tree.LazySegmentTree.ceilLog2

import scala.reflect.ClassTag
import scala.runtime.Nothing$


trait Operation[Operand, Op]:
  def merge(newOperation: Op, oldOperation: Op): Op
  def apply(operation: Op, operand: Operand): Operand
  val zero: Op

class LazySegmentTree[T: ClassTag, F: ClassTag](val size: Int)(using t: Monoid[T], f: Operation[T, F]):
  private val half = ceilLog2(size)
  private val vec = Array.fill(2 << half){t.zero}
  private val ops = Array.fill(1 << half){f.zero}
  private inline def applyChild(position: Int): Unit =
    val op = ops(position)
    ops(position) = f.zero
    val opLeft = f.merge(op, ops(position * 2))
    ops(position * 2) = opLeft
    vec(position * 2) = f.apply(op, vec(position * 2))
    val opRight = f.merge(op, ops(position * 2 + 1))
    ops(position * 2 + 1) = opRight
    vec(position * 2 + 1) = f.apply(op, vec(position * 2 + 1))
  private inline def applyPosition(position: Int): Unit =
    val pos = position + (1 << half)
    for d <- half to 2 by -1 do
      applyChild(pos >> d)
    val op = ops(pos >> 1)
    ops(pos >> 1) = f.zero
    vec(pos) = f.apply(op, vec(pos))
    vec(pos ^ 1) = f.apply(op, vec(pos ^ 1))
  private inline def updatePosition(position: Int): Unit =
    val pos = position + (1 << half)
    for d <- 1 to half do
      updateChild(pos >> d)
  private inline def applyAll(from: Int, until: Int): Unit =
    val left = from + (1 << half)
    val right = until + (1 << half)
    for d <- half to 2 by -1 do
      if (left >> d) << d != left then
        applyChild(left >> d)
      if (right >> d) << d != right then
        applyChild(right >> d)
    if (left & 1) == 1 then
      val op = ops(left >> 1)
      ops(left >> 1) = f.zero
      vec(left) = f.apply(op, vec(left))
      vec(left ^ 1) = f.apply(op, vec(left ^ 1))
    if (right & 1) == 1 then
      val op = ops(right >> 1)
      ops(right >> 1) = f.zero
      vec(right) = f.apply(op, vec(right))
      vec(right ^ 1) = f.apply(op, vec(right ^ 1))
  private inline def updateChild(position: Int): Unit =
    assert(ops(position) == f.zero)
    vec(position) = t.plus(vec(position * 2), vec(position * 2 + 1))
  private inline def updateAll(from: Int, until: Int): Unit =
    val left = from + (1 << half)
    val right = until + (1 << half)
    for d <- 1 to half do
      if (left >> d) << d != left then
        updateChild(left >> d)
      if (right >> d) << d != right then
        updateChild(right >> d)
  def get(from: Int, until: Int): T =
    applyAll(from, until)
    var leftResult = t.zero
    var rightResult = t.zero
    var left = from + (1 << half)
    var right = until + (1 << half)
    while left < right do
      if (left & 1) == 1 then
        leftResult = t.plus(leftResult, vec(left))
        left += 1
      if (right & 1) == 1 then
        right -= 1
        rightResult = t.plus(vec(right), rightResult)
      left >>= 1
      right >>= 1
    t.plus(leftResult, rightResult)
  def get(position: Int): T =
    applyPosition(position)
    vec(position + (1 << half))
  def apply(from: Int, until: Int, op: F): Unit =
    applyAll(from, until)
    var left = from + (1 << half)
    var right = until + (1 << half)
    if (left & 1) == 1 then
      vec(left) = f.apply(op, vec(left))
      left += 1
    if (right & 1) == 1 then
      right -= 1
      vec(right) = f.apply(op, vec(right))
    left >>= 1
    right >>= 1
    while left < right do
      if (left & 1) == 1 then
        vec(left) = f.apply(op, vec(left))
        ops(left) = f.merge(op, ops(left))
        left += 1
      if (right & 1) == 1 then
        right -= 1
        vec(right) = f.apply(op, vec(right))
        ops(right) = f.merge(op, ops(right))
      left >>= 1
      right >>= 1
    updateAll(from, until)
  def apply(position: Int, op: F): Unit =
    applyPosition(position)
    vec(position + (1 << half)) = f.apply(op, vec(position + (1 << half)))
    updatePosition(position)
  def update(position: Int, value: T): Unit =
    applyPosition(position)
    vec(position + (1 << half)) = value
    updatePosition(position)

object LazySegmentTree:
  def ceilLog2(value: Int): Int =
    var result = 0
    for d <- 4 to 0 by -1 do
      if value >= (1 << (result + (1 << d))) then
        result += 1 << d
    if (1 << result) == value then
      result
    else
      result + 1