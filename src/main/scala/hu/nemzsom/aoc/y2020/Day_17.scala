package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.Solver

object Day_17 extends App with Solver {

  override def solve(input: List[String]) = {
    val space = Space(input, (x, y) => Coord3D(x, y, 0))
    Range(0, 6).foldLeft(space)((s, _) => s.nextRound()).activeCubes.size.toString
  }

  override def solveSecondPart(input: List[String]) = {
    val space = Space(input, (x, y) => Coord4D(x, y, 0, 0))
    Range(0, 6).foldLeft(space)((s, _) => s.nextRound()).activeCubes.size.toString
  }

  trait Coord {
    def neighbours(): List[Coord]
  }

  case class Coord3D(x: Int, y: Int, z: Int) extends Coord {

    def neighbours(): List[Coord3D] = (for {
      xDiff <- Range.inclusive(-1, 1)
      yDiff <- Range.inclusive(-1, 1)
      zDiff <- Range.inclusive(-1, 1)
      if !(xDiff == 0 && yDiff == 0 && zDiff == 0)
    } yield Coord3D(x + xDiff, y + yDiff, z + zDiff)).toList
  }

  case class Coord4D(x: Int, y: Int, z: Int, w: Int) extends Coord {

    def neighbours(): List[Coord4D] = (for {
      xDiff <- Range.inclusive(-1, 1)
      yDiff <- Range.inclusive(-1, 1)
      zDiff <- Range.inclusive(-1, 1)
      wDiff <- Range.inclusive(-1, 1)
      if !(xDiff == 0 && yDiff == 0 && zDiff == 0 && wDiff == 0)
    } yield Coord4D(x + xDiff, y + yDiff, z + zDiff, w + wDiff)).toList
  }

  case class Space(activeCubes: Set[Coord]) {

    def calculateActive(coord: Coord): Option[Coord] = Some(coord).filter(_ => List(2, 3).contains(activeNeighbours(coord)))
    def calculateInActive(coord: Coord): Option[Coord] = Some(coord).filter(_ => activeNeighbours(coord) == 3)

    def activeNeighbours(coord: Coord): Int = coord.neighbours().count(activeCubes.contains)

    def nextRound(): Space = Space(
      activeCubes
        .flatMap(coord => coord.neighbours().toSet + coord)
        .map(coord =>
          if (activeCubes.contains(coord)) calculateActive(coord)
          else calculateInActive(coord)
        ).filter(_.isDefined)
        .map(_.get)
    )
  }

  object Coord3D {

    def rangeFor(activeCubes: Set[Coord3D], dimension: Coord3D => Int): Range = {
      val actives = activeCubes.map(dimension)
      Range.inclusive(actives.min, actives.max)
    }

    def toString(activeCubes: Set[Coord3D]): String = {
      def toStringForZ(z: Int): String = {
        rangeFor(activeCubes, _.x).map(x => rangeFor(activeCubes, _.y).map(y =>
          if (activeCubes.contains(Coord3D(x, y, z))) '#'
          else '.'
        ).mkString("")).mkString(System.lineSeparator())
      }
      rangeFor(activeCubes, _.z)
        .map(z => s"z=$z${System.lineSeparator()}" + toStringForZ(z) + System.lineSeparator())
        .mkString(System.lineSeparator())
    }
  }

  object Space {
    def apply(lines: List[String], cubeCreator: (Int, Int) => Coord): Space = Space(
      lines.zipWithIndex.flatMap { case (line, y) =>
        line.zipWithIndex.map { case (ch, x) =>
          if (ch == '#') Some(cubeCreator(x, y))
          else None
        }
      }.filter(_.isDefined)
        .map(_.get)
        .toSet
    )
  }

  solve()
}
