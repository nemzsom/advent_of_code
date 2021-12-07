package hu.nemzsom.aoc

trait IntLines extends Parser {

  def solve(input: List[String]): Any = solveInts(parseToInts(input))
  def solveSecondPart(input: List[String]): Any = solveIntsSecondPart(parseToInts(input))

  def solveInts(input: List[Int]): Any
  def solveIntsSecondPart(input: List[Int]): Any

}
