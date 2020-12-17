package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.y2020.Day_16.Input
import hu.nemzsom.aoc.{Solver, Tester}

class Day_16Spec extends Tester {
  override def solver: Solver = Day_16

  override def input: String =
    """class: 1-3 or 5-7
      |row: 6-11 or 33-44
      |seat: 13-40 or 45-50
      |
      |your ticket:
      |7,1,14
      |
      |nearby tickets:
      |7,3,47
      |40,4,50
      |55,2,20
      |38,6,12""".stripMargin

  override val expectedResult: String = "71"

  override def expectedResultPart2: String = ???

  val part2Example =
    """class: 0-1 or 4-19
      |row: 0-5 or 8-19
      |seat: 0-13 or 16-19
      |
      |your ticket:
      |11,12,13
      |
      |nearby tickets:
      |3,9,18
      |15,1,5
      |5,14,9""".stripMargin

  "field order" should "match the example" in {
    val orderedFields: List[Day_16.Field] = Day_16.findPositions(Input(asLines(part2Example)))
    assert(orderedFields.sorted.map(_.name) === List("row", "class", "seat"))
  }
}
