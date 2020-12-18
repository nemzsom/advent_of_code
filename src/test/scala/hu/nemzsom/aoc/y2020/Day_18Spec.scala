package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.{Solver, Tester}

class Day_18Spec extends Tester {
  override def solver: Solver = Day_18

  override def input: String =
    """1 + 2 * 3 + 4 * 5 + 6
      |1 + (2 * 3) + (4 * (5 + 6))
      |2 * 3 + (4 * 5)
      |5 + (8 * 3 + 9 + 3 * 4 * 3)
      |5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))
      |((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2""".stripMargin

  override val expectedResult: String = "26457"

  override def expectedResultPart2: String = "694173"
}
