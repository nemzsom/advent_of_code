package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.{Solver, Tester}

class Day_8Spec extends Tester {
  override def solver: Solver = Day_8

  override def input: String =
    """nop +0
      |acc +1
      |jmp +4
      |acc +3
      |jmp -3
      |acc -99
      |acc +1
      |jmp -4
      |acc +6""".stripMargin

  override val expectedResult: String = "5"

  override def expectedResultPart2: String = "8"
}
