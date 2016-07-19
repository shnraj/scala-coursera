package recfun

// Var vs val?
// Recursive return "Unit" - type mismatch
// Where do I not need "return"?

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) {
      1
    } else {
      pascal(c-1, r-1) + pascal(c, r-1)
    }
  }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance_helper(open: Int, close: Int, chars: List[Char]): Boolean = {
      if (!chars.isEmpty && (close <= open)) {
        if (chars.head == '(') balance_helper(open + 1, close, chars.tail)
        else if (chars.head == ')') balance_helper(open, close + 1, chars.tail)
        else balance_helper(open, close, chars.tail)
      } else if (chars.isEmpty && open == close) {
        println(chars.isEmpty)
        true
      } else {
        false
      }
    }
    balance_helper(0, 0, chars)
  }

    /**
     * Exercise 3
     */
    def countChange(money: Int, coins: => List[Int]): Int = {
      def helper(money: Int, coins: List[Int]): Int = {
        val l = for (coin <- coins.sorted) yield {
          if (money - coin == 0) 1
          else if (money - coin < 0) 0
          else helper(money - coin, coins.dropWhile(_ != coin))
        }
        l.sum
      }
      helper(money, coins)
    }

  def countChange2(money: Int, coins: List[Int]): Int = {
    val z = coins map { coin =>
      (money - coin) match {
        case 0 => 1
        case negative if negative < 0 => 0
        case remainder => countChange2(remainder, coins.dropWhile(_ != coin))
      }
    }
    z.sum
  }

  def countChange3(money: Int, coins: => List[Int]): Int = {
    if (money < 0)
      0

    coins.foldLeft(0) { (accum, coin) =>
      (money - coin) match {
        case 0 => accum + 1
        case remainder => accum + countChange3(remainder, coins.dropWhile(_ != coin))
      }
    }
  }
}
