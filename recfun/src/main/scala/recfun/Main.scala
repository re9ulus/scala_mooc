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

      def checkBalance(chars: List[Char], acc: Int): Boolean = {
        if (chars.isEmpty) return acc == 0
        if (chars.head == '(') return checkBalance(chars.tail, acc+1)
        if (chars.head == ')') {
          if (acc > 0) return checkBalance(chars.tail, acc-1)
          else return false
        }
        checkBalance(chars.tail, acc)
      }
      checkBalance(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) return 1
      if (money < 0 || coins.isEmpty) return 0
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
