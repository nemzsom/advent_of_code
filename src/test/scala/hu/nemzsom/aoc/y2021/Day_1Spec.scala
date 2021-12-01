package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.{Solver, Tester}

class Day_1Spec extends Tester {
  override def solver: Solver = Day_1

  override def input: String =
    """199
      |200
      |208
      |210
      |200
      |207
      |240
      |269
      |260
      |263""".stripMargin

  override val expectedResult: String = "7"
  override def expectedResultPart2: String = "5"
}
