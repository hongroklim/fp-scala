package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Var(b()*b() - 4*a()*c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    def calc(a: Double, b: Double, delta: Double): Set[Double] =
      if (delta < 0) Set()
      else (Set((-1*b + scala.math.sqrt(delta)) / (2*a),
                (-1*b - scala.math.sqrt(delta)) / (2*a)))

    Var(calc(a(), b(), delta()))
  }
}
