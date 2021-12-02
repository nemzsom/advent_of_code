package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.{Solver, Tester}

class Day_2Spec extends Tester {
  override def solver: Solver = Day_2

  override def input: String =
    """forward 5
      |down 5
      |forward 8
      |up 3
      |down 8
      |forward 2""".stripMargin

  override val expectedResult: String = "150"
  override def expectedResultPart2: String = "900"
}
