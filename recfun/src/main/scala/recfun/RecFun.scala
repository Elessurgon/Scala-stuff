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
    def iter(c: Int, r: Int): Int = {
      if (c == 0 || r == c) 1 else {
        iter(c - 1, r - 1) + iter(c, r - 1)
      }
    }

    iter(c, r)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @scala.annotation.tailrec
    def accountParen(num: Int, chars: List[Char], endParen: Boolean): Boolean = {
      if (chars.isEmpty) {
        if (num == 0) endParen
        else false
      }
      else if (chars.head == '(') accountParen(num + 1, chars.tail, endParen = false)
      else if (chars.head == ')') accountParen(num - 1, chars.tail, endParen = true)
      else accountParen(num, chars.tail, endParen)
    }

    accountParen(0, chars, endParen = false)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else {
      countChange(money - coins.head, coins) +
        countChange(money, coins.tail)
    }
  }
}
