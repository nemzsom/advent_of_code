package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.Solver

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day_17 extends App with Solver {

  type Coord = (Int, Int)
  implicit class CoordFunctions(coord: Coord) {
    def x: Int = coord._1
    def y: Int = coord._2
    def next(xVelocity: Int, yVelocity: Int): Coord = (x + xVelocity, y + yVelocity)
  }

  case class Probe(xVelocity: Int, yVelocity: Int, pos: Coord = (0, 0)) {
    def next: Probe = Probe(if (xVelocity > 0) xVelocity - 1 else 0, yVelocity - 1, pos.next(xVelocity, yVelocity))

    @tailrec
    final def untilVerticalFall: Probe =
      if (xVelocity == 0) this
      else next.untilVerticalFall

    @tailrec
    final def shoot(target: Target, yMax: Int = 0): Option[Int] = {
      if (target.isInside(pos)) Some(yMax)
      else if (target.isOverShot(pos)) None
      else next.shoot(target, if (pos.y > yMax) pos.y else yMax)
    }
  }

  case class Target(xRange: Range, yRange: Range) {
    def isInside(coord: Coord): Boolean = xRange.contains(coord.x) && yRange.contains(coord.y)
    def isOverShot(coord: Coord): Boolean = coord.x > xRange.max || coord.y < yRange.start
  }
  object Target {
    val regex: Regex = """target area: x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)""".r
    def apply(spec: String): Target = {
      spec match {
        case regex(xFrom, xTo, yFrom, yTo) => Target(xFrom.toInt to xTo.toInt, yFrom.toInt to yTo.toInt)
      }
    }
  }

  def validShoots(target: Target): Seq[Int] = {

    def calcXMinMax(target: Target): Range = {
      def isInReach(xStartVelocity: Int) = Probe(xStartVelocity, 0).untilVerticalFall.pos.x > target.xRange.start
      LazyList.from(1).filter(isInReach).head to target.xRange.end
    }

    val xMinMax: Range = calcXMinMax(target)
    val yMin = target.yRange.start
    val yMax = 1000 // high enough
    Range(yMin, yMax)
      .flatMap(yVelocity => xMinMax.map(xVelocity => Probe(xVelocity, yVelocity)))
      .flatMap(_.shoot(target))
  }

  override def solve(input: List[String]) = validShoots(Target(input.head)).max
  override def solveSecondPart(input: List[String]) = validShoots(Target(input.head)).length

  solve()
}
