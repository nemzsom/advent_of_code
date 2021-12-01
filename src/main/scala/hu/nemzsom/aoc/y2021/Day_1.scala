package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.{IntLines, Solver}

object Day_1 extends App with Solver with IntLines {

  override def solveInts(input: List[Int]) =
    input.sliding(2)
      .map { case a :: b :: Nil => if (b > a) 1 else 0}
      .sum.toString

  override def solveIntsSecondPart(input: List[Int]) = solveInts(
    input.sliding(3).map(_.sum).toList
  )

  solve()
}
