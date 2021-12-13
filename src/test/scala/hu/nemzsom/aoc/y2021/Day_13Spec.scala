package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.{Solver, Tester}

class Day_13Spec extends Tester {
  override def solver: Solver = Day_13

  override def input: String =
    """6,10
      |0,14
      |9,10
      |0,3
      |10,4
      |4,11
      |6,0
      |6,12
      |4,1
      |0,13
      |10,12
      |3,4
      |3,0
      |8,4
      |1,10
      |2,14
      |8,10
      |9,0
      |
      |fold along y=7
      |fold along x=5""".stripMargin

  override val expectedResult: String = "17"
  override def expectedResultPart2: String = """#####
                                               |#...#
                                               |#...#
                                               |#...#
                                               |#####
                                               |.....
                                               |.....""".stripMargin
}
