package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.{Solver, Tester}

class Day_12Spec extends Tester {
  override def solver: Solver = Day_12

  override def input: String =
    """start-A
      |start-b
      |A-c
      |A-b
      |b-d
      |A-end
      |b-end""".stripMargin

  override val expectedResult: String = "10"
  override def expectedResultPart2: String = "36"


  def largerInput: String =
    """dc-end
      |HN-start
      |start-kj
      |dc-start
      |dc-HN
      |LN-dc
      |HN-end
      |kj-sa
      |kj-HN
      |kj-dc""".stripMargin

  "The solution for part1 for larger input" should "match the example" in {
      assert(solver.solve(asLines(largerInput)).toString === "19")
  }

  "The solution for part2 for larger input" should "match the example" in {
      assert(solver.solveSecondPart(asLines(largerInput)).toString === "103")
  }

  def evenLargerInput: String =
    """fs-end
      |he-DX
      |fs-he
      |start-DX
      |pj-DX
      |end-zg
      |zg-sl
      |zg-pj
      |pj-he
      |RW-he
      |fs-DX
      |pj-RW
      |zg-RW
      |start-pj
      |he-WI
      |zg-he
      |pj-fs
      |start-RW""".stripMargin

  "The solution for part1 for the even larger input" should "match the example" in {
    assert(solver.solve(asLines(evenLargerInput)).toString === "226")
  }

  "The solution for part2 for the even larger input" should "match the example" in {
    assert(solver.solveSecondPart(asLines(evenLargerInput)).toString === "3509")
  }
}
