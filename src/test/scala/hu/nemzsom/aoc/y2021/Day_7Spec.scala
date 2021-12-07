package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.{Solver, Tester}

class Day_7Spec extends Tester {
  override def solver: Solver = Day_7

  override def input: String =
    """16,1,2,0,4,2,7,1,2,14""".stripMargin

  override val expectedResult: String = "37"
  override def expectedResultPart2: String = "168"
}
