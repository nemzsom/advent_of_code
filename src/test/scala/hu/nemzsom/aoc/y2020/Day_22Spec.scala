package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.{Solver, Tester}

class Day_22Spec extends Tester {
  override def solver: Solver = Day_22

  override def input: String =
    """Player 1:
      |9
      |2
      |6
      |3
      |1
      |
      |Player 2:
      |5
      |8
      |4
      |7
      |10""".stripMargin

  override val expectedResult: String = "306"

  override def expectedResultPart2: String = "291"
}
