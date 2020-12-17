package hu.nemzsom.aoc

trait LongLines {

  def solve(input: List[String]): String = {
      solveInts(input.map(s => s.toLong))
  }

  def solveSecondPart(input: List[String]): String = {
    solveIntsSecondPart(input.map(s => s.toLong))
  }

  def solveInts(input: List[Long]): String

  def solveIntsSecondPart(input: List[Long]): String

}
