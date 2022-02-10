package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.{Solver, Tester}

class Day_17Spec extends Tester {
  override def solver: Solver = Day_17

  override def input: String = "target area: x=20..30, y=-10..-5"

  override val expectedResult: String = "45"
  override def expectedResultPart2: String = "112"

}
