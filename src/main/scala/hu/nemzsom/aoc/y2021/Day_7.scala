package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.{OneLineInts, Solver}

import java.lang.Math.abs
import scala.annotation.tailrec

object Day_7 extends App with Solver with OneLineInts {

  def fuelCost(positions: List[Int], targetPos: Int)(costF: Int => Int): Int =
    positions.foldLeft(0)((cost, crabPos) => cost + costF(abs(targetPos - crabPos)))

  def minFuelCost(positions: List[Int])(costF: Int => Int): Int =
    Range.inclusive(positions.min, positions.max).map(targetPos => fuelCost(positions, targetPos)(costF)).min

  def flatCost(distance: Int): Int = distance
  @tailrec
  def increasingCost(distance: Int, totalCost: Int = 0): Int = distance match {
    case 0 => totalCost
    case _ => increasingCost(distance - 1, totalCost + distance)
  }

  override def solveInts(input: List[Int]) = minFuelCost(input)(flatCost)
  override def solveIntsSecondPart(input: List[Int]) = minFuelCost(input)(distance => increasingCost(distance))
  solve()
}
