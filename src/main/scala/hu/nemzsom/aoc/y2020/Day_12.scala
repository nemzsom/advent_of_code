package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.Solver

import scala.util.matching.Regex

object Day_12 extends App with Solver {

  //                 (0, 1)
  //   N              y ↑
  // W   E              |
  //   S    (-1, 0) ←---|---→ (1, 0)
  //                    |   x
  //                    ↓
  //                 (0, -1)

  override def solve(input: List[String]) =
    input.map(Instruction(_))
      .foldLeft(Ship1(Pos(0, 0), (1, 0)))((ship, instruction) => instruction.execute(ship))
      .manhattanDistance()
      .toString

  override def solveSecondPart(input: List[String]) =
    input.map(Instruction(_))
      .foldLeft(Ship2(Pos(0, 0), Pos(10, 1)))((ship, instruction) => instruction.execute(ship))
      .manhattanDistance()
      .toString

  case class Instruction(c: Char, value: Int){
    def execute(ship: Ship1): Ship1 = c match {
      case 'N' => ship.move(value, (0, 1))
      case 'S' => ship.move(value, (0, -1))
      case 'E' => ship.move(value, (1, 0))
      case 'W' => ship.move(value, (-1, 0))
      case 'L' => ship.turn(Left(value))
      case 'R' => ship.turn(Right(value))
      case 'F' => ship.move(value)
    }

    def execute(ship: Ship2): Ship2 = c match {
      case 'N' => ship.copy(waypoint = ship.waypoint.calc((0, 1), value))
      case 'S' => ship.copy(waypoint = ship.waypoint.calc((0, -1), value))
      case 'E' => ship.copy(waypoint = ship.waypoint.calc((1, 0), value))
      case 'W' => ship.copy(waypoint = ship.waypoint.calc((-1, 0), value))
      case 'L' => ship.turn(Left(value))
      case 'R' => ship.turn(Right(value))
      case 'F' => ship.move(value)
    }
  }

  object Instruction {
    val pattern: Regex = "([NSEWLRF])(\\d+)".r
    def apply(str: String): Instruction = {
      val pattern(c, value) = str
      new Instruction(c.charAt(0), value.toInt)
    }
  }

  case class Pos(x: Int, y: Int){
    def calc(direction: (Int, Int), distance: Int): Pos =
      Pos(x + direction._1 * distance, y + direction._2 * distance)
  }
  trait Ship {
    def pos: Pos
    def manhattanDistance(): Int = Math.abs(pos.x) + Math.abs(pos.y)
  }
  case class Ship1(pos: Pos, direction: (Int, Int)) extends Ship {

    def move(distance: Int, direction: (Int, Int) = this.direction): Ship1 =
      this.copy(pos = pos.calc(direction, distance))

    def turn(to: Either[Int, Int], count: Int = 1): Ship1 = {
      if (count == 0) this
      else to match {
        case Right(90) => this.copy(direction = (direction._2, - direction._1)).turn(to, count - 1)
        case Right(180) => turn(Right(90), 2)
        case Right(270) => turn(Right(90), 3)
        case Left(90) => turn(Right(270))
        case Left(180) => turn(Right(180))
        case Left(270) => turn(Right(90))
      }
    }
  }

  case class Ship2(pos: Pos, waypoint: Pos) extends Ship {
    def move(distance: Int): Ship2 =
      this.copy(pos = pos.calc((waypoint.x, waypoint.y), distance))

    def turn(to: Either[Int, Int], count: Int = 1): Ship2 = {
      if (count == 0) this
      else to match {
        case Right(90) => this.copy(waypoint = Pos(waypoint.y, - waypoint.x)).turn(to, count - 1)
        case Right(180) => turn(Right(90), 2)
        case Right(270) => turn(Right(90), 3)
        case Left(90) => turn(Right(270))
        case Left(180) => turn(Right(180))
        case Left(270) => turn(Right(90))
      }
    }
  }

  solve()
}
