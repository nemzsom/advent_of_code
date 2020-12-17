package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.{Solver, Tester}

class Day_1Spec extends Tester {
  override def solver: Solver = Day_1

  override def input: String =
    """1721
      |979
      |366
      |299
      |675
      |1456""".stripMargin

  override val expectedResult: String = "514579"
  override def expectedResultPart2: String = "241861950"
}
