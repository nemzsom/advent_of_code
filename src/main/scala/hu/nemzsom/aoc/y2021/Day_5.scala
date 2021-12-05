package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.Solver

import java.lang.Math.max
import scala.util.matching.Regex

object Day_5 extends App with Solver {

  case class Line(from: (Int, Int), to: (Int, Int)) {
    def isVertical: Boolean = from._1 == to._1
    def isHorizontal: Boolean = from._2 == to._2
    def isStraight: Boolean = isVertical || isHorizontal
    def getPoints: Seq[(Int, Int)] =
      if (isHorizontal) {
        val step = if (from._1 < to._1) 1 else -1
        Range.inclusive(from._1, to._1, step).map(x => (x, from._2))
      } else if (isVertical) {
        val step = if (from._2 < to._2) 1 else -1
        Range.inclusive(from._2, to._2, step).map(y => (from._1, y))
      } else {
        val xStep = if (from._1 < to._1) 1 else -1
        val yStep = if (from._2 < to._2) 1 else -1
        Range.inclusive(from._1, to._1, xStep)
          .zip(Range.inclusive(from._2, to._2, yStep))
      }
  }

  case class Floor(points: Array[Array[Int]]) {
    def mark(line: Line): Unit = line.getPoints.foreach {
      case (x, y) => points(y)(x) = points(y)(x) + 1
    }
    def overlapCount: Int = points.flatten.count(_ > 1)
    override def toString: String = points.map(_.toList).toList.mkString("\n")
  }
  object Floor {
    def apply(xMax: Int, yMax: Int): Floor = Floor(Array.ofDim[Int](xMax + 1, yMax + 1))
  }

  def parse(input: List[String]): List[Line] = {
    val line: Regex = """(\d+),(\d+) -> (\d+),(\d+)""".r
    input.map {
      case line(fromX, fromY, toX, toY) => Line((fromX.toInt, fromY.toInt), (toX.toInt, toY.toInt))
    }
  }

  def overlaps(lines: List[Line]): Int = {
    val dimension = lines.flatMap(line => List(line.from, line.to)).reduce[(Int, Int)]{
      case ((x1, y1), (x2, y2)) => (max(x1, x2), max(y1, y2))
    }
    val floor = Floor(dimension._1, dimension._2)
    lines.foreach(floor.mark)
    floor.overlapCount
  }

  override def solve(input: List[String]) = overlaps(parse(input).filter(_.isStraight))

  override def solveSecondPart(input: List[String]) = overlaps(parse(input))
  solve()
}
