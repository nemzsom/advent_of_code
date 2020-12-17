package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.{Solver, Tester}

class Day_14Spec extends Tester {
  override def solver: Solver = Day_14

  override def input: String =
    """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
      |mem[8] = 11
      |mem[7] = 101
      |mem[8] = 0""".stripMargin

  override val expectedResult: String = "165"

  override def input2: String =
    """mask = 000000000000000000000000000000X1001X
      |mem[42] = 100
      |mask = 00000000000000000000000000000000X0XX
      |mem[26] = 1""".stripMargin

  override def expectedResultPart2: String = "208"

}
