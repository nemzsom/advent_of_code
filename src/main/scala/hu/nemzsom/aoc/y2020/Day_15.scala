package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.Solver

object Day_15 extends App with Solver {

  override def solve(input: List[String]) = compute(input, 2020)
  override def solveSecondPart(input: List[String]) = compute(input, 30000000)


  def compute(input: List[String], round: Int): String = {
    val (numbers, last) = parseInput(input)
    val startingMap = initMap(numbers) // Map[number, round]

    val (_, result) = Range.inclusive(numbers.size + 2, round).foldLeft((startingMap, last)) { case ((map, lastNumber), round) =>
      val current = map.get(lastNumber)
        .map(lastRound => round - lastRound - 1)
        .getOrElse(0)
      (map.updated(lastNumber, round - 1), current)
    }
    result.toString
  }

  def initMap(numbers: List[Int]): Map[Int, Int] =
    numbers.zipWithIndex.foldLeft(Map[Int, Int]()) { case (map, (number, i)) =>
      map.updated(number, i + 1)
    }

  def parseInput(input: List[String]): (List[Int], Int) = {
    val numbers = input.head.split(",").map(_.toInt).toList
    (numbers.take(numbers.size - 1), numbers.last)
  }

  solve()
}
