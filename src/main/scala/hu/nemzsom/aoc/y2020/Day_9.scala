package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.{LongLines, Solver}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day_9 extends App with Solver with LongLines {

  override def solveLongs(input: List[Long]) = Day_9(25).solveLongs(input)

  override def solveLongsSecondPart(input: List[Long]) = Day_9(25).solveLongsSecondPart(input)

  solve()
}

case class Day_9(preambleSize: Int) extends Solver with LongLines {
  override def solveLongs(input: List[Long]): Long = {
    val (preamble, list) =  input.splitAt(preambleSize)
    findFirstValid(Queue(preamble: _*), list)
  }

  def findFirstValid(preamble: Queue[Long], list: List[Long]): Long = list match {
    case head :: _ if !valid(preamble, head) => head
    case head :: tail => findFirstValid(preamble.dequeue._2.enqueue(head), tail)
  }

  def valid(preamble: Seq[Long], num: Long): Boolean = {
    val permutations = for {
      x <- preamble
      y <- preamble
      if x != y
      if x + y == num
    } yield (x, y)
    permutations.nonEmpty
  }

  override def solveLongsSecondPart(input: List[Long]): Long = {
    val numberToFind = solveLongs(input)
    @tailrec
    def findSequence(acc: List[Long], list: List[Long]): Long = list match {
      case head :: _ if acc.sum + head == numberToFind => (head :: acc).max + (head :: acc).min
      case head :: tail if acc.sum + head < numberToFind => findSequence(head :: acc, tail)
      case _ => -1
    }
    @tailrec
    def find(list: List[Long]): Long = {
      val res = findSequence(List(), list)
      if (res == -1) find(list.tail)
      else res
    }
    find(input)
  }
}
