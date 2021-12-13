package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.Solver

object Day_13 extends App with Solver {

  type Point = (Int, Int)
  type Paper = Array[Array[Boolean]]
  type Instructions = List[Fold]
  case class Fold(pos: Int, horizontal: Boolean)

  implicit class PointFunctions(p: Point) {
    def x: Int = p._1
    def y: Int = p._2
  }

  implicit class PaperFunctions(paper: Paper) {
    def mark(p: Point): Paper = {
      paper(p.y)(p.x) = true
      paper
    }
    def countPoints: Int = paper.map(_.count(identity)).sum
    def fold(f: Fold): Paper = f match {
      case Fold(pos, true) => // horizontal
        val (top, bottom) = paper.splitAt(pos)
        top.zip(bottom.tail.reverse).map(rows =>
          rows._1.zip(rows._2).map(marks => marks._1 || marks._2))
      case Fold(pos, false) => // vertical
        paper.map { row =>
          val (left, right) = row.splitAt(pos)
          left.zip(right.tail.reverse).map(marks => marks._1 || marks._2)
        }
    }

    def toDots: String = paper.map(_.map(mark => if (mark) "#" else ".").mkString).mkString("\n")
  }

  def parse(input: List[String]): (Paper, Instructions) = {
    val pointPattern = """(\d+),(\d+)""".r
    val foldPattern = """fold along ([xy])=(\d+)""".r
    val (paperDef, instructionsDef) = input.splitAt(input.indexOf(""))
    val points = paperDef.map {
      case pointPattern(x, y) => (x.toInt, y.toInt)
    }
    val xMax = points.map(_.x).max
    val yMax = points.map(_.y).max
    val paper = points.foldLeft(Array.ofDim[Boolean](yMax + 1, xMax + 1))((paper, p) => paper.mark(p))
    val instructions = instructionsDef.tail.map {
      case foldPattern(axis, pos) => Fold(pos.toInt, horizontal = axis == "y")
    }
    (paper, instructions)
  }

  override def solve(input: List[String]) = {
    val (paper, instructions) = parse(input)
    paper.fold(instructions.head).countPoints
  }
  override def solveSecondPart(input: List[String]) = {
    val (paper, instructions) = parse(input)
    instructions.foldLeft(paper)((paper, instruction) => paper.fold(instruction)).toDots
  }
  solve()
}
