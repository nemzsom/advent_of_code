package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.{Solver, Tester}

class Day_17Spec extends Tester {
  override def solver: Solver = Day_17

  override def input: String =
    """.#.
      |..#
      |###""".stripMargin

  override val expectedResult: String = "112"

  override def expectedResultPart2: String = "848"
}
