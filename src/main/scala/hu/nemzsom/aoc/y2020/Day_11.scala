package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.Solver

import scala.annotation.tailrec

object Day_11 extends App with Solver {

  override def solve(input: List[String]): String =
    runSimulations(input, _.transform((place, pos, area) => place.check(pos, area)))

  def runSimulations(input: List[String], transform: Area => Area): String = {
    val area = Area(input.map(_.map(Place(_)).toVector).toVector)
    @tailrec
    def run(area: Area): Area = {
      val newArea = transform(area)
      if (newArea == area) area
      else run(newArea)
    }
    run(area).countOccupied().toString
  }

  override def solveSecondPart(input: List[String]) =
    runSimulations(input, _.transform((place, pos, area) => place.check2(pos, area)))

  solve()

  case class Pos(x: Int, y: Int) {
    def next(direction: (Int, Int)): Pos = Pos(x + direction._1, y + direction._2)
  }
  sealed trait Place {
    val directions: List[(Int, Int)] = for {
      x <- List(-1, 0, 1)
      y <- List(-1, 0, 1)
      if x != 0 || y != 0
    } yield (x, y)
    def isOccupied: Boolean = false
    def isSeat: Boolean = true
    def check(pos: Pos, area: Area): Place
    def check2(pos: Pos, area: Area): Place
    def adjacent(pos: Pos, area: Area): List[Place] = {
      val positions = for {
        x <- List(-1, 0, 1).map(_ + pos.x)
        y <- List(-1, 0, 1).map(_ + pos.y)
        if pos != Pos(x, y)
      } yield Pos(x, y)
      positions.map(area.get)
    }
    def visible(pos: Pos, area: Area): List[Place] =
      directions.map(direction => area.findVisible(pos.next(direction), direction))

  }

  case object Occupied extends Place {
    override def toString = "#"
    override val isOccupied = true

    override def check(pos: Pos, area: Area): Place = {
      if (adjacent(pos, area).count(_.isOccupied) >= 4) Empty
      else this
    }

    override def check2(pos: Pos, area: Area): Place = {
      if (visible(pos, area).count(_.isOccupied) >= 5) Empty
      else this
    }
  }
  case object Empty extends Place {
    override def toString = "L"

    override def check(pos: Pos, area: Area): Place = {
      if (adjacent(pos, area).exists(place => place.isOccupied)) this
      else Occupied
    }

    override def check2(pos: Pos, area: Area): Place = {
      if (visible(pos, area).exists(place => place.isOccupied)) this
      else Occupied
    }
  }
  case object Floor extends Place {
    override def toString = "."
    override val isSeat: Boolean = false

    override def check(pos: Pos, area: Area): Place = this
    override def check2(pos: Pos, area: Area): Place = this
  }

  case class Area(seats: Vector[Vector[Place]], width: Int, height: Int) {
    override def toString: String = seats.map(_.mkString("")).mkString(System.lineSeparator())
    def get(pos: Pos): Place =
      if (isOutside(pos)) Floor
      else seats(pos.y)(pos.x)

    def transform(check: (Place, Pos, Area) => Place): Area = {
      val seats = allPositions().map(_.map(pos => check(get(pos), pos, this)))
      Area(seats, width, height)
    }

    @tailrec
    final def findVisible(pos: Pos, direction: (Int, Int)): Place = pos match {
      case p if isOutside(p) => Floor
      case p if get(p).isSeat => get(p)
      case p => findVisible(p.next(direction), direction)
    }

    def countOccupied(): Int = seats.flatten.count(_.isOccupied)

    private def isOutside(pos: Pos) =
      pos.x < 0 || pos.x >= width ||
        pos.y < 0 || pos.y >= height

    private def allPositions(): Vector[Vector[Pos]] =
      Range(0, height).map(y => Range(0, width).map(x => Pos(x, y)).toVector).toVector
  }

  object Area {
    def apply(seats: Vector[Vector[Place]]): Area = Area(seats, seats(0).size, seats.size)
  }

  object Place {
    def apply(literal: Char): Place = literal match {
      case '#' => Occupied
      case 'L' => Empty
      case '.' => Floor
    }
  }
}
