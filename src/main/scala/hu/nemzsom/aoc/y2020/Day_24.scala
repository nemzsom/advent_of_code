package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.Solver

import scala.collection.mutable
import scala.util.matching.Regex

object Day_24 extends App with Solver {

  type Steps = List[Direction]

  override def solve(input: List[String]) = {
    val pane = flipPane(Input.parse(input))
    pane.countBlackTiles.toString
  }

  override def solveSecondPart(input: List[String]) = {
    val pane = flipPane(Input.parse(input))
    Range(0, 100).foreach(_ => pane.nextPattern())
    pane.countBlackTiles.toString
  }

  def flipPane(in: Input): Pane = {
    val plane = Pane()
    in.flips.foreach(plane.flip)
    plane
  }

  sealed trait Direction {
    def pos: Pos
  }
  object Direction {
    def apply(s: String): Direction = s match {
      case "e" => East
      case "se" => SouthEast
      case "sw" => SouthWest
      case "w" => West
      case "nw" => NorthWest
      case "ne" => NorthEast
    }
  }
  case object East extends Direction { val pos: Pos = Pos(1, 0, -1)}
  case object SouthEast extends Direction { val pos: Pos = Pos(0, 1, -1)}
  case object SouthWest extends Direction { val pos: Pos = Pos(-1, 1, 0)}
  case object West extends Direction { val pos: Pos = Pos(-1, 0, 1)}
  case object NorthWest extends Direction { val pos: Pos = Pos(0, -1, 1)}
  case object NorthEast extends Direction { val pos: Pos = Pos(1, -1, 0)}
  case class Pos(x: Int, y: Int, z: Int) {
    def +(pos: Pos): Pos = Pos(x + pos.x, y + pos.y, z + pos.z)
    def neighbours: Set[Pos] = Set(East, SouthEast, SouthWest, West, NorthWest, NorthEast).map(this + _.pos)
  }

  object Pos {
    val center: Pos = Pos(0, 0, 0)
  }

  case class Pane(blackTiles: mutable.Set[Pos] = mutable.Set()) {

    def flip(steps: Steps): Unit = {
      val endPos = steps.foldLeft(Pos.center){ case (pos, direction) =>
        pos + direction.pos
      }
      if (blackTiles.contains(endPos)) blackTiles.remove(endPos)
      else blackTiles.add(endPos)
    }

    def nextPattern(): Unit = {
      val nextBlacks = blackTiles.flatMap(pos => pos.neighbours + pos).flatMap(pos =>
        if (isBlack(pos)) calculateBlack(pos)
        else calculateWhite(pos)
      )
      blackTiles.clear()
      blackTiles.addAll(nextBlacks)
    }

    def calculateBlack(black: Pos): Option[Pos] = {
      val bn = blackNeighbours(black)
      if (bn == 0 || bn > 2) None
      else Some(black)
    }
    def calculateWhite(white: Pos): Option[Pos] =
      if (blackNeighbours(white) == 2) Some(white)
      else None

    def isBlack(pos: Pos): Boolean = blackTiles.contains(pos)
    def blackNeighbours(pos: Pos): Int = pos.neighbours.count(isBlack)
    def countBlackTiles: Int = blackTiles.size
  }

  case class Input(flips: List[Steps])
  object Input {

    val pattern: Regex = "(e|se|sw|w|nw|ne)".r

    def parse(in: List[String]): Input = Input(
      in.map(line => pattern.findAllIn(line).map(Direction(_)).toList)
    )
  }

  solve()
}
