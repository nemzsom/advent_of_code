package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.{Solver, Tester}

class Day_6Spec extends Tester {
  override def solver: Solver = Day_6

  override def input: String =
    """3,4,3,1,2""".stripMargin

  override val expectedResult: String = "5934"
  override def expectedResultPart2: String = "26984457539"
}
