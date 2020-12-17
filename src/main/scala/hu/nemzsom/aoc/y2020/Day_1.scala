package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.{IntLines, Solver}

object Day_1 extends App with Solver with IntLines {

  override def solveInts(input: List[Int]) = {
    val combinations = for {
      x <- input
      y <- input
    } yield (x, y)
    combinations
      .filter { case (x, y) => x + y == 2020}
      .map{ case (x, y) => x * y}
      .head
      .toString
  }

  override def solveIntsSecondPart(input: List[Int]) = {
    val combinations = for {
      x <- input
      y <- input
      z <- input
    } yield (x, y, z)
    combinations
      .filter { case (x, y, z) => x + y + z == 2020}
      .map{ case (x, y, z) => x * y * z}
      .head
      .toString
  }

  solve()
}
