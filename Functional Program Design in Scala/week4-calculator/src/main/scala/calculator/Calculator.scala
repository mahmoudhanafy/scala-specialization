package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.mapValues { value =>
      Signal(eval(value(), namedExpressions))
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {

    def evaluate(expr: Expr, visited: Set[String]): Double = {
      expr match {
        case Literal(v: Double) =>  v

        case Ref(name: String) =>
          if (visited(name)) Double.NaN else evaluate(getReferenceExpr(name, references), visited + name)

        case Plus(a: Expr, b: Expr) =>
          evaluate(a, visited) + evaluate(b, visited)

        case Minus(a: Expr, b: Expr) =>
          evaluate(a, visited) - evaluate(b, visited)

        case Times(a: Expr, b: Expr) =>
          evaluate(a, visited) * evaluate(b, visited)

        case Divide(a: Expr, b: Expr) =>
          evaluate(a, visited) / evaluate(b, visited)
      }
    }

    evaluate(expr, Set())
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]): Expr = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
