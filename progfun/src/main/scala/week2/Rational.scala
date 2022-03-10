package week2

class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")
  // assert(y != 0)
  
  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)
  def numer = x / g
  def denom = y / g

  def + (that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)

  def neg = new Rational(-1 * numer, denom)

  def sub(that: Rational) = this + that.neg 

  def mul(that: Rational) =
    new Rational(numer * that.numer, denom * that.denom)

  def rev = new Rational(denom, numer)

  def div(that: Rational) = mul(that.rev)

  def less(that: Rational) = numer * that.denom < that.numer * denom
  
  def max(that: Rational) = if (this.less(that)) that else this

  override def toString = s"$numer / $denom"
}
