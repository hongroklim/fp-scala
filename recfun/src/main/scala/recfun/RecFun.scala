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

    def leftMoney(money: Int, i: Int, head: Int) =
      money - (i * head)
    
    def isCharged(money: Int, i: Int, head: Int) =
      if (leftMoney(money, i, head) == 0) 1 else 0

    def getIndex(money: Int, head: Int): Int =
      if (head == 0) 0
      else (money - (money % head)) / head

    def getNextHead(coins: List[Int]) =
      if (coins.tail.isEmpty) 0
      else coins.tail.head

    def loop(money: Int, i: Int, coins: List[Int]): Int =
      if (money == 0 || i < 0 || coins.isEmpty) 0 
      else isCharged(money, i, coins.head) +
            loop(money, i - 1, coins) + // Next changes
            loop(leftMoney(money, i, coins.head),
                 getIndex(leftMoney(money, i, coins.head),
                          getNextHead(coins)),
                 coins.tail)           // Decrease head's count
    
    if (coins.isEmpty) 0
    else loop(money, getIndex(money, coins.head), coins)
  }
}
