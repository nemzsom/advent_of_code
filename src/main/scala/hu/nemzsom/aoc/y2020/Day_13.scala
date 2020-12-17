package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.Solver

import scala.annotation.tailrec

object Day_13 extends App with Solver {

  override def solve(input: List[String]) = {
    val (now, ids) = parse(input)
    val next: Int => Int = nextDepart(now)
    val (id, nextTime) = ids.map(id => (id, next(id))).min(Ordering.by[(Int, Int), Int](_._2))
    (id * (nextTime - now)).toString
  }

  def parse(input: List[String]): (Int, List[Int]) = {
    val nowStr :: buses :: Nil = input
    val now = nowStr.toInt
    val ids = buses.split(',').filter(!_.equals("x")).map(_.toInt).toList
    (now, ids)
  }

  def nextDepart(now: Int)(busId: Int) = {
    val leftSince = now % busId
    val left = now -leftSince
    left + busId
  }

  override def solveSecondPart(input: List[String]) = {
    val buses = parse2(input)
    find(buses, Acc(0, 1)).time.toString
  }

  @tailrec
  def find(buses: List[Bus], acc: Acc): Acc = buses match {
    case head :: tail if isSatisfied(acc.time, head) => find(tail, acc.found(head.id))
    case head :: _ => find(buses, acc.next)
    case Nil => acc
  }

  def isSatisfied(time: Long, bus: Bus) = isDepartureTime(time + bus.offset, bus.id)

  def isDepartureTime(time: Long, id: Long) = time % id == 0

  def parse2(input: List[String]): List[Bus] = {
    val _ :: buses :: Nil = input
    buses.split(",").zipWithIndex
      .filter{ case (id, _) => !id.equals("x")}
      .map{ case (id, offset) => Bus(id.toLong, offset)}
      .toList
  }

  case class Bus(id: Long, offset: Long)

  case class Acc(time: Long, step: Long) {
    def found(id: Long): Acc = this.copy(step = this.step * id)
    def next: Acc = this.copy(time = this.time + step)
  }

  solve()
}
