package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.{Solver, Tester}

class Day_2Spec extends Tester {
  override def solver: Solver = Day_2

  override def input: String =
    """1-3 a: abcde
      |1-3 b: cdefg
      |2-9 c: ccccccccc""".stripMargin

  override val expectedResult: String = "2"
  override def expectedResultPart2: String = "1"
}
