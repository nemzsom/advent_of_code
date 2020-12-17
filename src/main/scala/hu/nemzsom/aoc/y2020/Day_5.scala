package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.Solver
import hu.nemzsom.aoc.y2020.Day_1.solve

object Day_5 extends App with Solver {

  override def solve(input: List[String]) =
    getPasses(input).map(_.id()).max.toString

  override def solveSecondPart(input: List[String]) = {
    val allPasses = (for {
      row <- Range(0, 128)
      column <- Range(0, 8)
    } yield BPass(row, column)).toSet
    val passes = getPasses(input).toSet
    val ids = passes.map(_.id())
    (allPasses -- passes)
      .filter(p => ids.contains(p.id() + 1) && ids.contains(p.id() - 1))
      .head
      .id()
      .toString
  }

  def getPasses(input: List[String]) = input.map(BPass(_))

  case class BPass(row: Int, column: Int) {
    def id(): Int = row * 8 + column
  }

  object BPass {
    def apply(code: String): BPass = {
      val (rowStr, columnStr) = code.map{
        case 'F' => '0'
        case 'B' => '1'
        case 'L' => '0'
        case 'R' => '1'
      }.splitAt(7)
      BPass(Integer.parseInt(rowStr, 2), Integer.parseInt(columnStr, 2))
    }
  }

  solve()
}
