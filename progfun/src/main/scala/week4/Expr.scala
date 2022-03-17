package week4

trait Expr
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Var(s: String) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr

object ExprApp extends App {
  def show(e: Expr): String = e match {
    case Number(x) => x.toString
    case Sum(l, r) => show(l) + " + " + show(r)
    case Var(x) => x
    case Prod(l, r) => {
      def bindShow(e: Expr): String =
        if (e.isInstanceOf[Sum]) "(" + show(e) + ")"
        else show(e)

      bindShow(l) + " * " + bindShow(r)
    }
  }

  println(show(Sum(Number(1), Number(44))))
  println(show(Sum(Prod(Number(2), Var("x")), Var("y"))))
  println(show(Prod(Sum(Number(2), Var("x")), Var("y"))))
}
