package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.{Solver, Tester}

class Day_12Spec extends Tester {
  override def solver: Solver = Day_12

  override def input: String =
    """F10
      |N3
      |F7
      |R90
      |F11""".stripMargin

  override val expectedResult: String = "25"

  override def expectedResultPart2: String = "286"
}
