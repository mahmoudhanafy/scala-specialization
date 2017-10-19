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
      if (c == 0 || r == 0 || r == c)
        1
      else
        pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def isGood(open: Int, cs: List[Char]): Boolean = {
        cs match {
          case c :: ls =>
            c match {
              case ')' if open == 0 =>
                false
              case ')' =>
                isGood(open - 1, ls)
              case '(' =>
                isGood(open + 1, ls)
              case _ =>
                isGood(open, ls)
            }

          case Nil =>
            open == 0
        }
      }

      isGood(0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      coins match {
        case coin :: ls if money >= coin =>
          val takeIt = countChange(money - coin, coins)
          val leaveIt = countChange(money, ls)
          takeIt + leaveIt

        case _ :: ls =>
          countChange(money, ls)

        case Nil if money == 0 =>
          1
        case Nil =>
          0
      }
    }

  }
