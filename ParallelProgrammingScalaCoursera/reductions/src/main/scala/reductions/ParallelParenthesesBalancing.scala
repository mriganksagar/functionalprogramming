package reductions

import scala.annotation.*
import org.scalameter.*
import scala.collection.mutable.Stack
import scala.runtime.stdLibPatches.language.`3.2`

object ParallelParenthesesBalancingRunner:

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns := 40,
    Key.exec.maxWarmupRuns := 80,
    Key.exec.benchRuns := 120,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit =
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

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface:

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean =
    var count = 0
    for(c <- chars) do {
      if c == '(' then count += 1
      else if c == ')' then if count > 0 then count -=1 else return false
      else ()
    }
    count == 0

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean =

    @tailrec
    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      if idx == until then (arg1, arg2)
      else if chars(idx) == '(' then traverse(idx+1, until, arg1+1, arg2)
      else if chars(idx) == ')' then 
        if arg1 > 0 then traverse(idx+1, until, arg1-1, arg2)
        else traverse(idx+1, until, arg1, arg2+1)
      else traverse(idx+1, until, arg1, arg2)
    }

    def reduce(from: Int, until: Int): (Int, Int)= {
      if threshold >= until - from then traverse(from, until, 0, 0)
      else {
        val mid = from + (until - from)/2
        val (p1, p2) = parallel(reduce(from, mid), reduce(mid, until))
        if p1._2 > p2._1 then (p1._1, p2._2 + p1._2 - p2._1)
        else (p1._1 + p2._1 - p1._2, p2._2)
      }
    }

    reduce(0, chars.length) == (0, 0)

  // For those who want more:
  // Prove that your reduction operator is associative!

