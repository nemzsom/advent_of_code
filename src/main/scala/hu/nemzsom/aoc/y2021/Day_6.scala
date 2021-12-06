package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.Solver

import java.lang.Math.max
import scala.annotation.tailrec
import scala.util.matching.Regex

object Day_6 extends App with Solver {
  
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
  }

  override def solve(input: List[String]) = LSchool.parse(input.head).simulate(80)
  override def solveSecondPart(input: List[String]) = LSchool.parse(input.head).simulate(256)
  solve()
}
