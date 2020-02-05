package exercises

trait Expr
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr

object ExprTest extends App {
  def toHumanString(e: Expr): String =
    e match {
      case Number(n) => s"$n"
      case Sum(x, y) => s"${toHumanString(x)} + ${toHumanString(y)}"
      case Prod(x: Sum, y: Sum) => s"(${toHumanString(x)}) * (${toHumanString(y)})"
      case Prod(x: Sum, y) => s"(${toHumanString(x)}) * ${toHumanString(y)}"
      case Prod(x, y: Sum) => s"${toHumanString(x)} * (${toHumanString(y)})"
      case Prod(x, y) => s"${toHumanString(x)} * ${toHumanString(y)}"
    }

  assert(toHumanString(Number(2)) == "2")
  assert(toHumanString(Sum(Number(2), Number(3))) == "2 + 3")
  assert(toHumanString(Prod(Sum(Number(2), Number(3)), Number(4))) == "(2 + 3) * 4")
  assert(toHumanString(Prod(Number(4), Sum(Number(2), Number(3)))) == "4 * (2 + 3)")
  assert(toHumanString(Prod(Sum(Number(1), Number(4)), Sum(Number(2), Number(3)))) == "(1 + 4) * (2 + 3)")
  assert(toHumanString(Prod(Number(1), Number(4))) == "1 * 4")
}
