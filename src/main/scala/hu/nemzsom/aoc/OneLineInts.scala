package hu.nemzsom.aoc

trait OneLineInts extends Parser {

  def solve(input: List[String]): Any = solveInts(parseFirstLineAsInts(input))
  def solveSecondPart(input: List[String]): Any = solveIntsSecondPart(parseFirstLineAsInts(input))

  def solveInts(input: List[Int]): Any
  def solveIntsSecondPart(input: List[Int]): Any

}
