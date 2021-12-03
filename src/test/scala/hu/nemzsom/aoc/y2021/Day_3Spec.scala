package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.{Solver, Tester}

class Day_3Spec extends Tester {
  override def solver: Solver = Day_3

  override def input: String =
    """00100
      |11110
      |10110
      |10111
      |10101
      |01111
      |00111
      |11100
      |10000
      |11001
      |00010
      |01010""".stripMargin

  override val expectedResult: String = "198"
  override def expectedResultPart2: String = "230"
}
