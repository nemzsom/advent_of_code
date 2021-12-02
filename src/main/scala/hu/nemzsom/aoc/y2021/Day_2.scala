package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.Solver

import scala.util.matching.Regex

object Day_2 extends App with Solver {

  sealed trait Command {
    def move(pos: Pos): Pos
    def move2(pos: Pos): Pos
  }
  case class Forward(units: Int) extends Command {
    override def move(pos: Pos): Pos = Pos(pos.horizontal + units, pos.depth)
    override def move2(pos: Pos): Pos = Pos(pos.horizontal + units, pos.depth + pos.aim * units, pos.aim)
  }
  case class Up(units: Int) extends Command {
    override def move(pos: Pos): Pos = Pos(pos.horizontal, pos.depth - units)
    override def move2(pos: Pos): Pos = Pos(pos.horizontal, pos.depth, pos.aim - units)
  }
  case class Down(units: Int) extends Command {
    override def move(pos: Pos): Pos = Pos(pos.horizontal, pos.depth + units)
    override def move2(pos: Pos): Pos = Pos(pos.horizontal, pos.depth, pos.aim + units)
  }

  object Command {
    val command: Regex = """(\w+) (\d+)""".r
    def apply(s: String): Command = s match {
      case command("forward", unit) => Forward(unit.toInt)
      case command("up", unit) => Up(unit.toInt)
      case command("down", unit) => Down(unit.toInt)
    }
  }

  case class Pos(horizontal: Int, depth: Int, aim: Int) {
    def result: String = (horizontal * depth).toString
  }
  object Pos {
    def apply(horizontal: Int, depth: Int): Pos = Pos(horizontal, depth, 0)
  }

  def executeCommands(input: List[String])(interpretation: (Pos, Command) => Pos): String =
    input.map(Command(_))
      .foldLeft(Pos(0, 0, 0))(interpretation(_, _))
      .result

  override def solve(input: List[String]) = executeCommands(input)((pos, command) => command.move(pos))

  override def solveSecondPart(input: List[String]) = executeCommands(input)((pos, command) => command.move2(pos))

  solve()
}
