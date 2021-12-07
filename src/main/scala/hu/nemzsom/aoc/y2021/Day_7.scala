package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.Solver

import java.lang.Math.abs
import scala.annotation.tailrec

object Day_7 extends App with Solver {

  def fuelCost(positions: List[Int], targetPos: Int): Int =
    positions.foldLeft(0)((cost, crabPos) => cost + abs(targetPos - crabPos))

  def fuelCost2(positions: List[Int], targetPos: Int): Int =
    positions.foldLeft(0)((cost, crabPos) => cost + fuelBurn(crabPos, targetPos))

  @tailrec
  def fuelBurn(from: Int, to: Int, sumCost: Int = 0, stepCost: Int = 1): Int = {
    if (from < to) fuelBurn(to, from, sumCost, stepCost)
    else if (from == to) sumCost
    else fuelBurn(from - 1, to, sumCost + stepCost, stepCost + 1)
  }

  def minFuelCost(positions: List[Int]): Int = {
    Range.inclusive(positions.min, positions.max).map(targetPos => fuelCost(positions, targetPos)).min
  }

  def minFuelCost2(positions: List[Int]): Int = {
    Range.inclusive(positions.min, positions.max).map(targetPos => fuelCost2(positions, targetPos)).min
  }

  def parse(input: List[String]): List[Int] = input.head.split(",").map(_.toInt).toList

  override def solve(input: List[String]) = minFuelCost(parse(input))
  override def solveSecondPart(input: List[String]) = minFuelCost2(parse(input))
  solve()
}
