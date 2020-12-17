package hu.nemzsom.aoc

trait IntLines {

  def solve(input: List[String]): String = {
      solveInts(input.map(s => s.toInt))
  }

  def solveSecondPart(input: List[String]): String = {
    solveIntsSecondPart(input.map(s => s.toInt))
  }

  def solveInts(input: List[Int]): String

  def solveIntsSecondPart(input: List[Int]): String

}
