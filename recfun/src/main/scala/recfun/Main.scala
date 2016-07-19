package recfun

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
    * Exercise 1: Pascal’s Triangle
    * The numbers at the edge of the triangle are all 1, and each number
    * inside the triangle is the sum of the two numbers above it.
    * Write a function that computes the elements of Pascal’s triangle
    * by means of a recursive process.
    *
    * Do this exercise by implementing the pascal function in Main.scala,
    * which takes a column c and a row r, counting from 0 and returns the
    * number at that spot in the triangle.
    * For example, pascal(0,2)=1,pascal(1,2)=2 and pascal(1,3)=3.
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) {
      1
    } else {
      pascal(c-1, r-1) + pascal(c, r-1)
    }
  }
  
  /**
    * Exercise 2: Parentheses Balancing
    * Write a recursive function which verifies the balancing of parentheses
    * in a string, which we represent as a List[Char] not a String.
    * For example, the function should return true for the following strings:
    *   (if (zero? x) max (/ 1 x))
    *   I told him (that it’s not (yet) done). (But he wasn’t listening)
    * The function should return false for the following strings:
    *   :-)
    *   ())(
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
    * Exercise 3: Counting Change
    * Write a recursive function that counts how many different ways you can
    * make change for an amount, given a list of coin denominations.
    * For example, there are 3 ways to give change for 4 if you have coins
    * with denomination 1 and 2: 1+1+1+1, 1+1+2, 2+2.
    *
    * Do this exercise by implementing the countChange function inMain.scala.
    * This function takes an amount to change, and a list of
    * unique denominations for the coins.
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

  // Alternative solution (using match)
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

  // Alternative solution (using foldLeft)
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
