package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.Solver

object Day_3 extends App with Solver {

  override def solve(input: List[String]) =
    countTrees(input, Path(3, 1)).toString

  override def solveSecondPart(input: List[String]) =
    List(
      Path(1, 1),
      Path(3, 1),
      Path(5, 1),
      Path(7, 1),
      Path(1, 2)
    ).map(countTrees(input, _)).product.toString

  def countTrees(input: List[String], path: Path) = {
    val patternLength = input.head.length
    input.foldLeft(Acc(0, 0, 0)) { case (acc, line) =>
      val remainder = acc.pos % patternLength
      val filterLine = acc.line % path.down != 0
      if (filterLine)
        acc.skipLine()
      else if (line.charAt(remainder) == '#')
        acc.treeFound(path.right)
      else
        acc.noTree(path.right)
    }
  }.trees


  case class Acc(line: Int, pos: Int, trees: Long) {
    def treeFound(goRight: Int) = Acc(line + 1, pos + goRight, trees + 1)
    def noTree(goRight: Int) = Acc(line + 1, pos + goRight, trees)
    def skipLine() = Acc(line + 1, pos, trees)
  }

  case class Path(right: Int, down: Int)

  solve()
}
