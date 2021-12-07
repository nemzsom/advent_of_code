package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.{OneLineInts, Solver}

import scala.annotation.tailrec

object Day_6 extends App with Solver with OneLineInts {
  
  case class LSchool(fish: Map[Int, Long]) {

    @tailrec
    final def simulate(days: Int): Long = {
      if (days == 0) {
        fish.values.sum
      } else {
        val updated = fish.map {
          case (timer, count) => (timer - 1, count)
        }
        val born = updated.getOrElse(-1, 0L)
        val next = (updated.removed(-1) + (8 -> born)).updatedWith(6) {
          case Some(count) => Some(count + born)
          case None => Some(born)
        }
        LSchool(next).simulate(days - 1)
      }
    }
  }
  object LSchool {
    def parse(literal: String): LSchool =
      LSchool(literal.split(",")
        .map(_.toInt)
        .groupBy(timer => timer)
        .view.mapValues(_.length.toLong).toMap)

    def apply(fish: List[Int]): LSchool =
      LSchool(fish
        .groupBy(timer => timer)
        .view.mapValues(_.length.toLong).toMap)
  }

  override def solveInts(input: List[Int]) = LSchool(input).simulate(80)
  override def solveIntsSecondPart(input: List[Int]) = LSchool(input).simulate(256)
  solve()
}
