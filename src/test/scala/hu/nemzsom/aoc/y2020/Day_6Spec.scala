package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.{Solver, Tester}

class Day_6Spec extends Tester {
  override def solver: Solver = Day_6

  override def input: String =
    """abc
      |
      |a
      |b
      |c
      |
      |ab
      |ac
      |
      |a
      |a
      |a
      |a
      |
      |b""".stripMargin

  override val expectedResult: String = "11"

  override def expectedResultPart2: String = "6"
}
