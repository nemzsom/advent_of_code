package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.Solver

import scala.annotation.tailrec

object Day_3 extends App with Solver {

  type Binary = List[Int]

  private def toBinary(s: String): Binary = s.toCharArray.map(_.asDigit).toList

  private def sumDigits(binaries: List[String]): List[Int] = binaries
    .map(toBinary)
    .reduce((binA, binB) => binA.zip(binB).map { case (a, b) => a + b })

  private def parseBinary(binary: Binary): Int =
    Integer.parseInt(binary.mkString(""), 2)

  private def mostCommon(sums: List[Int], size: Int): Binary = sums.map(x => if (x >= size / 2.0) 1 else 0)
  private def leastCommon(sums: List[Int], size: Int): Binary = sums.map(x => if (x < size / 2.0) 1 else 0)

  override def solve(input: List[String]): Int = {
    val sums = sumDigits(input)
    val gammaBinary = mostCommon(sums, input.size)
    val epsilonBinary = leastCommon(sums, input.size)
    parseBinary(gammaBinary) * parseBinary(epsilonBinary)
  }

  @tailrec
  def findRating(input: List[String], position: Int)(criteria: (List[Int], Int) => Binary): Int =
    if (input.size == 1) {
      parseBinary(toBinary(input.head))
    } else {
      val sums = sumDigits(input)
      val keep = criteria(sums, input.size)(position).toString.charAt(0)
      findRating(input.filter(_.charAt(position) == keep), position + 1)(criteria)
    }

  override def solveSecondPart(input: List[String]) =
    findRating(input, 0)(mostCommon) * findRating(input, 0)(leastCommon)

  solve()
}
