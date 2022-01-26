package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.{Solver, Tester}

class Day_15Spec extends Tester {
  override def solver: Solver = Day_15

  override def input: String =
    """1163751742
      |1381373672
      |2136511328
      |3694931569
      |7463417111
      |1319128137
      |1359912421
      |3125421639
      |1293138521
      |2311944581""".stripMargin

  override val expectedResult: String = "40"
  override def expectedResultPart2: String = "315"
}
