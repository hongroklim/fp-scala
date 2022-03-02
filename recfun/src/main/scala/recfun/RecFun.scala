package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def calc(stack: Int, head: Char) =
      if (head == '(') stack + 1
      else (if (head == ')') stack - 1 else stack)

    def loop(acc: Int, remnant: List[Char]): Int =
      if (remnant.isEmpty || acc < 0) acc 
      else loop(calc(acc, remnant.head), remnant.tail)

    loop(0, chars) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def subtract(balance: Int, coins: List[Int]): Int =
      if (coins.isEmpty) balance
      else subtract(balance - coins.head, coins.tail)
    
    def isAvailable(money: Int, coins: List[Int]) =
      subtract(money, coins) == 0

    
  }
}
