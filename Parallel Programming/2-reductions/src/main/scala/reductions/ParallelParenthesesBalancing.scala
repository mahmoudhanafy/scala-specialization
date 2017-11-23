package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var (count, min) = (0, 0)
    chars.foreach {
      case '(' =>
        count = count + 1
      case ')' =>
        count = count - 1
        min = math.min(min, count)
      case _ =>
    }

    count == 0 && min == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int): (Int, Int) = {
      var (open, min) = (0, 0)
      var currIdx = idx

      while (currIdx < until) {
        if (chars(currIdx) == '(')
          open = open + 1
        else if (chars(currIdx) == ')') {
          open = open - 1
          min = math.min(min, open)
        }

        currIdx = currIdx + 1
      }

      (open, min)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) {
        traverse(from, until)
      } else {
        val mid = from + (until - from) / 2
        val ((leftOpen, leftMin), (rightOpen, rightMin)) = parallel(reduce(from, mid), reduce(mid, until))

        val totalOpen = leftOpen + rightOpen
        val totalMin = math.min(leftMin, leftOpen + rightMin)
        (totalOpen, totalMin)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
