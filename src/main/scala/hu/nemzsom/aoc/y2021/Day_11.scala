package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.Solver
import hu.nemzsom.aoc.y2021.Day_11.Point.p

import scala.annotation.tailrec

object Day_11 extends App with Solver {

  case class Point(x: Int, y: Int) {
    lazy val adjacent: List[Point] = List(
      p(x + 1, y), p(x - 1, y), p(x, y + 1), p(x, y - 1),
      p(x + 1, y + 1), p(x - 1, y - 1), p(x - 1, y + 1), p(x + 1, y - 1))
      .filter(p => p.x >= 0 && p.y >= 0 && p.x < 10 && p.y < 10)
  }
  object Point {
    def p(x: Int, y: Int): Point = Point(x, y)
  }

  case class Octopuses(map: Array[Array[Int]]) {
    val allPoints: List[Point] =
      (for (x <- 0 until 10;
           y <- 0 until 10) yield p(x, y)).toList

    def next(): Int = {
      allPoints.foreach(increase)
      executeFlashes(allPoints).size
    }

    @tailrec
    final def executeFlashes(candidates: List[Point], flashed: Set[Point] = Set()): Set[Point] = candidates match {
      case Nil => flashed
      case p :: tail if flashed.contains(p) || getEnergy(p) < 10 => executeFlashes(tail, flashed)
      case p :: tail =>
        setEnergy(p, 0)
        val ajdNonFlashed = p.adjacent.filter(!flashed.contains(_))
        ajdNonFlashed.foreach(increase)
        executeFlashes((tail.toSet ++ ajdNonFlashed).toList, flashed + p)
    }

    def getEnergy(p: Point): Int = map(p.y)(p.x)
    def setEnergy(p: Point, e: Int): Unit = map(p.y)(p.x) = e
    def increase(p: Point): Unit = setEnergy(p, getEnergy(p) + 1)

    override def toString: String = map.map(_.mkString("")).mkString("\n")
  }
  object Octopuses {
    def apply(input: List[String]): Octopuses = Octopuses(input.map(_.toCharArray.map(_.asDigit)).toArray)
  }

  override def solve(input: List[String]) = {
    val octopuses = Octopuses(input)
    Range(0, 100).map(_ => octopuses.next()).sum
  }

  override def solveSecondPart(input: List[String]) = {
    val octopuses = Octopuses(input)
    LazyList.continually(octopuses.next()).zipWithIndex.find {
      case (sum, _) => sum == 100
    }.map(_._2 + 1).get
  }

  solve()
}
