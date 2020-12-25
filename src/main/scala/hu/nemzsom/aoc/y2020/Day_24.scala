package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.Solver

import scala.collection.mutable
import scala.util.matching.Regex

object Day_24 extends App with Solver {

  type Steps = List[Direction]

  override def solve(input: List[String]) = {
    val plane = Plane()
    Input.parse(input).flips.foreach(plane.flip)
    plane.countBlackTiles.toString
  }

  override def solveSecondPart(input: List[String]) = ???

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
  }

  object Pos {
    val center: Pos = Pos(0, 0, 0)
  }

  case class Plane(blackTiles: mutable.Set[Pos] = mutable.Set()) {

    def flip(steps: Steps): Unit = {
      val endPos = steps.foldLeft(Pos.center){ case (pos, direction) =>
        pos + direction.pos
      }
      if (blackTiles.contains(endPos)) blackTiles.remove(endPos)
      else blackTiles.add(endPos)
    }

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
