package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.{Solver, Tester}

class Day_25Spec extends Tester {
  override def solver: Solver = Day_25

  override def input: String =
    """5764801
      |17807724""".stripMargin

  override val expectedResult: String = "14897079"

  override def expectedResultPart2: String = ???
}
