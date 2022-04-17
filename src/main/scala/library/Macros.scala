package library

object Macros:
  import scala.quoted.*
  def inspectCode(x: Expr[Any])(using Quotes): Expr[Any] =
    println("In Macro")
    println(x.show)
    Expr({println("in block"); 5})
  inline def inspect(inline x: Any): Any = ${ inspectCode('x) }