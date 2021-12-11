package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.{Solver, Tester}

class Day_11Spec extends Tester {
  override def solver: Solver = Day_11

  override def input: String =
    """5483143223
      |2745854711
      |5264556173
      |6141336146
      |6357385478
      |4167524645
      |2176841721
      |6882881134
      |4846848554
      |5283751526""".stripMargin

  override val expectedResult: String = "1656"
  override def expectedResultPart2: String = "195"
}
