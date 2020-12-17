package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.{Solver, Tester}

class Day_9Spec extends Tester {
  override def solver: Solver = Day_9(5)

  override def input: String =
    """35
      |20
      |15
      |25
      |47
      |40
      |62
      |55
      |65
      |95
      |102
      |117
      |150
      |182
      |127
      |219
      |299
      |277
      |309
      |576""".stripMargin

  override def expectedResult: String = "127"

  override def expectedResultPart2: String = "62"

  "valid calculations" should "match the examples" in {
    val preamble = Range.Long.inclusive(1, 25, 1).toList
    val tested = Day_9(0)
    assert(tested.valid(preamble, 26) === true)
    assert(tested.valid(preamble, 49) === true)
    assert(tested.valid(preamble, 100) === false)
    assert(tested.valid(preamble, 50) === false)
  }

  it should "match also match with the next examples" in {
    val preamble: List[Long] = 45 :: Range.Long.inclusive(1, 25, 1).filter(_ != 20).toList
    val tested = Day_9(0)
    assert(tested.valid(preamble, 26) === true)
    assert(tested.valid(preamble, 65) === false)
    assert(tested.valid(preamble, 64) === true)
    assert(tested.valid(preamble, 66) === true)
  }
}
