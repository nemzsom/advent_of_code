package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.Solver

import java.lang.Math.{abs, random}
import scala.annotation.tailrec

object Day_7 extends App with Solver {

  def fuelCost(positions: List[Int], targetPos: Int)(costF: Int => Int): Int =
    positions.foldLeft(0)((cost, crabPos) => cost + costF(abs(targetPos - crabPos)))

  def minFuelCost(positions: List[Int])(costF: Int => Int): Int =
    Range.inclusive(positions.min, positions.max).map(targetPos => fuelCost(positions, targetPos)(costF)).min

  def parse(input: List[String]): List[Int] = input.head.split(",").map(_.toInt).toList

  def flatCost(distance: Int): Int = distance
  def increasingCost(distance: Int): Int = increasingCost(distance, 0)
  @tailrec
  def increasingCost(distance: Int, totalCost: Int): Int = distance match {
    case 0 => totalCost
    case _ => increasingCost(distance - 1, totalCost + distance)
  }

  override def solve(input: List[String]) = minFuelCost(parse(input))(flatCost)
  override def solveSecondPart(input: List[String]) = minFuelCost(parse(input))(increasingCost)
  solve()
}
