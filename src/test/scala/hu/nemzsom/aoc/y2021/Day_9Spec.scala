package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.{Solver, Tester}

class Day_9Spec extends Tester {
  override def solver: Solver = Day_9

  override def input: String =
    """2199943210
      |3987894921
      |9856789892
      |8767896789
      |9899965678""".stripMargin

  override val expectedResult: String = "15"
  override def expectedResultPart2: String = "1134"
}
