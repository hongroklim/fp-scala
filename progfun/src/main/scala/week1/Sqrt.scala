package week1

object Sqrt extends App {
  def abs(x: Double) = if(x < 0) -x else x
  
  def sqrt(x: Double) = {

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    /*
    // Previous
    def isGoodEnough(guess: Double, x: Double) = 
      abs(guess * guess - x) < 0.001
    */

    // Improved
    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.001
    

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }

  println(sqrt(2))
  println(sqrt(4))

  // Stress Test
  println(sqrt(1e-6))
  println(sqrt(1e60))
}
