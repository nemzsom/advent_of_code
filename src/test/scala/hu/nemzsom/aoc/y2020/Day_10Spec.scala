package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.{Solver, Tester}

class Day_10Spec extends Tester {
  override def solver: Solver = Day_10

  override def input: String =
    """16
      |10
      |15
      |5
      |1
      |11
      |7
      |19
      |6
      |12
      |4""".stripMargin

  override val expectedResult: String = "35"

  override def expectedResultPart2: String = "8"

  def largerInput =
  """28
    |33
    |18
    |42
    |31
    |14
    |46
    |20
    |48
    |47
    |24
    |23
    |49
    |45
    |19
    |38
    |39
    |11
    |1
    |32
    |25
    |35
    |8
    |17
    |7
    |9
    |4
    |2
    |34
    |10
    |3""".stripMargin

  "part2 answer" should "match the second example" in {
    println(asLines(input).map(_.toInt).sorted)
    val res = solver.solveSecondPart(asLines(largerInput))
    assert(res === "19208")
  }

  it should "calculate simple 3" in {
    val res = Day_10.solveIntsSecondPart(List(1, 2, 3))
    // (0), 1, 2, 3, (6)
    // (0), 1, 3, (6)
    // (0), 2, 3, (6)
    // (0), 3, (6)
    assert(res === "4")
  }

  it should "calculate simple 2" in {
    val res = Day_10.solveIntsSecondPart(List(1, 3))
    // (0), 1, 3, (6)
    // (0), 3, (6)
    assert(res === "2")
  }
}
