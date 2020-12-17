package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.Solver

object Day_6 extends App with Solver {

  override def solve(input: List[String]) =
    input.foldLeft(List(Set[Char]()))((groups, line) =>
      line match {
        case "" => Set[Char]() :: groups
        case chars => groups.updated(0, groups.head ++ chars)
      }).map(_.size).sum.toString

  override def solveSecondPart(input: List[String]) =
    input.foldLeft(List(List[String]()))((groups, line) =>
      line match {
        case "" => List[String]() :: groups
        case _ => groups.updated(0, line :: groups.head)
      })
      .map(_.map(_.toSet).reduce((p1, p2) => p1.intersect(p2)))
      .map(_.size).sum.toString

  solve()
}
