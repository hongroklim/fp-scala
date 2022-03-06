package clazz

class Rational(x: Int, y: Int) {
  def numer = x
  def denom = y

  def add(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)

  def neg = new Rational(-1 * numer, denom)

  def sub(that: Rational) = add(that.neg)

  def mul(that: Rational) =
    new Rational(numer * that.numer, denom * that.denom)

  def rev = new Rational(denom, numer)

  def div(that: Rational) = mul(that.rev)

  override def toString = s"$numer / $denom"
}
