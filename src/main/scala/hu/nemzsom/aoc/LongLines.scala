package hu.nemzsom.aoc

trait LongLines extends Parser {

  def solve(input: List[String]): Any = solveLongs(parseToLongs(input))
  def solveSecondPart(input: List[String]): Any = solveLongsSecondPart(parseToLongs(input))

  def solveLongs(input: List[Long]): Any
  def solveLongsSecondPart(input: List[Long]): Any

}
