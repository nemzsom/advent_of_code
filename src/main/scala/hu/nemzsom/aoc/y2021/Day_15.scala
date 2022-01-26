package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.Solver

import scala.annotation.tailrec

object Day_15 extends App with Solver {

  type Cavern = Array[Array[Int]]
  case class Coord(x: Int, y: Int) {
    def neighbors: List[Coord] = List(right, left, down, up)
    def right: Coord = Coord(x + 1, y)
    def left: Coord = Coord(x - 1, y)
    def down: Coord = Coord(x, y + 1)
    def up: Coord = Coord(x, y - 1)
  }

  implicit class RowFunctions(row: Array[Int]) {
    def inc(i: Int): Array[Int] = row.map(w => inc(w, i))
    private def inc(w: Int, i: Int): Int = {
      val res = w + i
      if (res >= 10) res - 9
      else res
    }
  }

  implicit class CavernFunctions(cavern: Cavern) {
    lazy val end: Coord = Coord(cavern.length - 1, cavern.length - 1)
    lazy val start: Coord = Coord(0, 0)
    def costAt(coord: Coord): Int = cavern(coord.y)(coord.x)
    def inside(coord: Coord): Boolean = coord.x >= 0 && coord.y >= 0 && coord.x < cavern.length && coord.y < cavern.length // cavern is a square
    def +(cavern2: Cavern): Cavern = cavern ++ cavern2
    def inc(i: Int): Cavern = cavern.map(_.inc(i))
    def inflate: Cavern = {
      val firstSection = cavern.map(row => row ++ row.inc(1) ++ row.inc(2) ++ row.inc(3) ++ row.inc(4))
      firstSection + firstSection.inc(1) + firstSection.inc(2) + firstSection.inc(3) + firstSection.inc(4)
    }
  }

  def parse(input: List[String]): Cavern =
    input.map(row => row.toCharArray.map(_.asDigit)).toArray

  def solve(cavern: Cavern): Int = {

    var checked: Set[Coord] = Set()

    @tailrec
    def findLowest(actual: Coord, visited: Map[Coord, Int], unvisited: Map[Coord, Int]): Int = {
      if (checked.contains(actual)) throw new IllegalStateException(s"$actual")
      checked = checked + actual
      val actCost = unvisited(actual)
      if (actual == cavern.end) {
        actCost
      } else {
        val updatedUnvisited = actual.neighbors
          .filter(c => !visited.contains(c))
          .filter(c => cavern.inside(c))
          .foldLeft(unvisited)((costs, neighbor) => {
            val cost = cavern.costAt(neighbor) + actCost
            costs.updatedWith(neighbor) {
              case None => Some(cost)
              case Some(prevCost) if prevCost > cost => Some(cost)
              case prevCost => prevCost
            }
          }).removed(actual)
        val next = updatedUnvisited.minBy{ case (_, cost) => cost }._1
        findLowest(next, visited + (actual -> actCost), updatedUnvisited)
      }
    }

    val unvisited = Map(cavern.start -> 0).withDefaultValue(Int.MaxValue)

    findLowest(cavern.start, Map(), unvisited)
  }

  override def solve(input: List[String]) = solve(parse(input))
  override def solveSecondPart(input: List[String]) = solve(parse(input).inflate)

  solve()
}
