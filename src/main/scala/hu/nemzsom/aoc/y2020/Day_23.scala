package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.Solver

import scala.annotation.tailrec

object Day_23 extends App with Solver {

  override def solve(input: List[String]) = CupCircle(parseInput(input.head)).iterate(100).labelsAfter1()

  override def solveSecondPart(input: List[String]) = {
    val firstCups = parseInput(input.head)
    val allCups = firstCups ++ Range.inclusive(firstCups.size + 1, 1_000_000)
    CupCircle(allCups).iterate(10_000_000).part2Result()
  }

  def parseInput(in: String): List[Int] = in.map(_.asDigit).toList

  case class Cup(label: Int, var next: Cup) {
    override def toString: String = s"Cup($label, Cup(${next.label}))"
  }
  case class CupCircle(cups: Array[Cup], var current: Cup) {

    def iterate(count: Int): CupCircle = {
      Range(0, count).foreach(_ => nextMove())
      this
    }

    private def nextMove(): CupCircle = {
      val nextThree = pickNextThree()
      val destinationCup = findDestinationCup(current.label, nextThree.map(_.label))
      nextThree.last.next = destinationCup.next
      destinationCup.next = nextThree.head
      current = current.next
      this
    }

    private def pickNextThree(): List[Cup] = {
      var c = current
      val nextThree = Range(0, 3).map { _ =>
        c = c.next
        c
      }.toList
      current.next = nextThree.last.next
      nextThree
    }

    @tailrec
    private def findDestinationCup(current: Int, nextThree: List[Int]): Cup = {
      val destination = current - 1
      if (destination < 1) findDestinationCup(cups.length, nextThree)
      else if (nextThree.contains(destination)) findDestinationCup(destination, nextThree)
      else cups(destination)
    }

    def labelsAfter1(): String = {
      var cup = cups(1).next
      Range(1, cups.length - 1).map { _ =>
        val label = cup.label
        cup = cup.next
        label.toString
      }.mkString("")
    }

    def part2Result(): String = {
      val cup1 = cups(1)
      val a = cup1.next.label
      val b = cup1.next.next.label
      (a.toLong * b.toLong).toString
    }
  }

  object CupCircle {
    def apply(initialOrder: List[Int]): CupCircle = {
      val cups = new Array[Cup](initialOrder.size + 1)
      val cupList = initialOrder.foldLeft(List[Cup]()){ case (cupList, label) =>
        val cup = Cup(label, null)
        cups(label) = cup
        if (cupList.nonEmpty) cupList.head.next = cup
        cup :: cupList
      }
      val last = cupList.head
      val current = cupList.last
      last.next = current
      CupCircle(cups, current)
    }
  }

  solve()
}
