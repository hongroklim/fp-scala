package reductions

import scala.annotation._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns := 40,
    Key.exec.maxWarmupRuns := 80,
    Key.exec.benchRuns := 120,
    Key.verbose := false
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def counter(char: Char): Int = char match {
      case '(' => 1
      case ')' => -1
      case _   => 0
    }

    def iter(idx: Int, acc: Int): Boolean =
      if (idx == chars.length) acc == 0
      else if (acc < 0) false
      else iter(idx+1, acc + counter(chars(idx)))

    iter(0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, accl: Int, accr: Int): (Int, Int) = {
      if (idx == until) (accl, accr)
      else chars(idx) match {
        case '(' => traverse(idx+1, until, accl+1, accr)
        case ')' => traverse(idx+1, until, accl, accr+1)
        case _   => traverse(idx+1, until, accl, accr)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold)
        traverse(from, until, 0, 0)
      else {
        val mid = from + (until - from)/2
        val ((ll, lr), (rl, rr)) = parallel(reduce(from, mid), reduce(mid+1, until))
        if (ll > rr) (ll + rl - rr, lr)
        else (rl, lr + rr - ll)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
