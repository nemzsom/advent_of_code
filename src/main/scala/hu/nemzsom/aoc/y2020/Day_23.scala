package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.Solver

object Day_23 extends App with Solver {

  override def solve(input: List[String]) = CupCircle(input.head).iterate(100).result()

  override def solveSecondPart(input: List[String]) = ???

  case class CupCircle(cups: LazyList[Int], round: Int) {

    def iterate(count: Int): CupCircle = LazyList.iterate(this)(_.nextMove()).take(count + 1).last

    private def nextMove(): CupCircle = {
      //println(s"-- move $round --")
      //println(s"cups: ${cups.take(9).toList}")
      val current = cups.head
      val nextThree = cups.tail.take(3)
      //println(s"pick up: ${nextThree.toList}")
      val remainedCircle = cups.filter(cup => !nextThree.contains(cup))
      val destinationCup = findDestinationCup(current, nextThree.toSet)
      //println(s"destination: $destinationCup")
      //println()
      //println(s"remainedCircle: ${remainedCircle.take(6).toList}")
      val destinationIndex = remainedCircle.indexOf(destinationCup) + 1
      //println(s"destinationIndex: $destinationIndex")
      val before = remainedCircle.take(destinationIndex).toList
      //println(s"before: $before")
      val after = remainedCircle.slice(destinationIndex, destinationIndex + (9 - 3 - before.size) + 1).toList
      //println(s"after: $after")
      CupCircle((before ++ nextThree ++ after).tail, round + 1)
    }

    private def findDestinationCup(current: Int, nextThree: Set[Int]): Int = {
      val destination = current - 1
      if (destination < 1) findDestinationCup(10, nextThree)
      else if (nextThree.contains(destination)) findDestinationCup(destination, nextThree)
      else destination
    }

    def result(): String =
      cups.dropWhile(_ != 1).tail.take(8).mkString
  }

  object CupCircle {
    def apply(cupStr: String): CupCircle = CupCircle(cupStr.map(_.asDigit).toList)

    def apply(cups: List[Int], round: Int = 1): CupCircle = CupCircle(LazyList.continually(cups).flatten, round)
  }

  solve()
}
