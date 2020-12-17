package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.{Solver, Tester}

class Day_13Spec extends Tester {
  override def solver: Solver = Day_13

  override def input: String =
    """939
      |7,13,x,x,59,x,31,19""".stripMargin

  override val expectedResult: String = "295"

  override def expectedResultPart2: String = "1068781"
}
