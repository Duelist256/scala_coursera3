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
    def loop(idx: Int, numOfOpen: Int): Boolean = {
      if (idx >= chars.length) numOfOpen == 0
      else if (numOfOpen < 0) false
      else chars(idx) match {
        case '(' => loop(idx + 1, numOfOpen + 1)
        case ')' => loop(idx + 1, numOfOpen - 1)
        case _ => loop(idx + 1, numOfOpen)
      }
    }
    loop(0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      if (idx < until)
        chars(idx) match {
          case '(' => traverse(idx + 1, until, arg1 + 1, arg2)
          case ')' if arg1 > 0 => traverse(idx + 1, until, arg1 - 1, arg2)
          case ')' if arg1 <= 0 => traverse(idx + 1, until, arg1, arg2 + 1)
          case _ => traverse(idx + 1, until, arg1, arg2)
        }
      else (arg1, arg2)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      val size = until - from
      if (size <= threshold) traverse(from, until, 0, 0)
      else {
        val mid = size / 2
        val ((a1, a2), (b1, b2)) = parallel(reduce(from, from + mid), reduce(from + mid, until))
        val min = Math.min(a1, b2)
        (a1 + b1 - min, a2 + b2 - min)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
