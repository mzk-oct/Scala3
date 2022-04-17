
import java.io.PrintWriter
import scala.collection.mutable.*
import scala.io.StdIn.*
import scala.util.chaining.*
import scala.math.*
import scala.reflect.ClassTag
import scala.util.*
import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.strictEquality

class A:
  def print(): Unit = println("A")
class B:
  def print(): Unit = println("B")

@main def main =
  val a = A()
  val b = B()
  println(a == b)