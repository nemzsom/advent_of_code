package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.y2020.Day_5.BPass
import hu.nemzsom.aoc.{Solver, Tester}

class Day_5Spec extends Tester {
  override def solver: Solver = Day_5

  override def input: String =
    """FBFBBFFRLR
      |BFFFBBFRRR
      |FFFBBBFRRR
      |BBFFBBFRLL""".stripMargin

  override val expectedResult: String = "820"

  override def expectedResultPart2: String = ???

  "BPass" should "calculate ids" in {
    var pass = BPass("FBFBBFFRLR")
    assert(pass === BPass(44, 5))
    assert(pass.id() === 357)

    pass = BPass("BFFFBBFRRR")
    assert(pass === BPass(70, 7))
    assert(pass.id() === 567)

    pass = BPass("FFFBBBFRRR")
    assert(pass === BPass(14, 7))
    assert(pass.id() === 119)

    pass = BPass("BBFFBBFRLL")
    assert(pass === BPass(102, 4))
    assert(pass.id() === 820)
  }
}
