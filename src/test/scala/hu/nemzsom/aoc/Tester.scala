package hu.nemzsom.aoc

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source
import scala.util.Try

abstract class Tester extends AnyFlatSpec {

  def solver: Solver
  def input: String
  def input2: String = input
  def expectedResult: String
  def expectedResultPart2: String = ???

  "The solution for part1" should "match the example" in {
    if (Try(expectedResult).isSuccess)
      assert(solver.solve(asLines(input)).toString === expectedResult)
  }

  "The solution for part2" should "match the example" in {
    if (Try(expectedResultPart2).isSuccess)
      assert(solver.solveSecondPart(asLines(input2)).toString === expectedResultPart2)
  }

  def asLines(str: String): List[String] = {
    Source.fromString(str).getLines().toList
  }

}
