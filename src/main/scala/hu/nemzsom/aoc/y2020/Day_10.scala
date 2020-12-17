package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.{IntLines, Solver}

import scala.collection.mutable

object Day_10 extends App with Solver with IntLines {

  override def solveInts(input: List[Int]) = {
    val (oneDiffs, threeDiffs, _) = input.sorted.foldLeft((0, 0, 0)){ case ((oneDiffs, threeDiffs, prev), jolt) =>
      jolt - prev match {
        case 1 => (oneDiffs+1, threeDiffs, jolt)
        case 3 => (oneDiffs, threeDiffs+1, jolt)
        case _ => (oneDiffs, threeDiffs, jolt)
      }
    }
    (oneDiffs * (threeDiffs + 1)).toString
  }

  override def solveIntsSecondPart(input: List[Int]) = {
    val memo: mutable.Map[Int, Long] = mutable.Map()
    def countCombinations(list: List[Int], jolt: Int): Long = {
      if (!memo.contains(jolt)) {
        val res = list match {
          case a :: b :: c :: tail if c - jolt <= 3 =>
            countCombinations(b :: c :: tail, a) + countCombinations(c :: tail, b) + countCombinations(tail, c)
          case a :: b :: tail if b - jolt <= 3 =>
            countCombinations(b :: tail, a) +  countCombinations(tail, b)
          case a :: tail =>
            countCombinations(tail, a)
          case Nil => 1
        }
        memo.update(jolt, res)
      }
      memo(jolt)
    }
    countCombinations(input.sorted, 0).toString
  }


  solve()
}
