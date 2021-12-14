package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.Solver

import scala.annotation.tailrec

object Day_14 extends App with Solver {

  type Polymer = List[Char]
  type Rules = Map[Pair, List[Pair]]
  type PairCount = (Pair, Long)
  case class Pair(a: Char, b: Char)

  implicit class RulesFunctions(rules: Rules) {
    def nextPairs(pair: Pair, count: Long): List[PairCount] = rules(pair).map(_ -> count)
  }

  def parse(input: List[String]): (Polymer, Rules) = (
    input.head.toCharArray.toList,
    input.drop(2).map(rule => Pair(rule(0), rule(1)) -> List(Pair(rule(0), rule(6)), Pair(rule(6), rule(1)))).toMap
  )

  @tailrec
  def polymerization(counts: Map[Pair, Long], rules: Rules, step: Int): Map[Pair, Long] =
    if (step == 0) counts else {
      val updated = counts.toList.flatMap {
        case (pair, count) => rules.nextPairs(pair, count)
      }.foldLeft(Map[Pair, Long]()){ case (counts, (pair, count)) =>
        counts.updatedWith(pair)(_.map(_ + count).orElse(Some(count)))
      }
      polymerization(updated, rules, step - 1)
    }

  def calculate(input: List[String], step: Int) = {
    val (polymer, rules) = parse(input)
    val initialCounts = polymer.sliding(2)
      .map { case a :: b :: Nil => Pair(a, b) -> 1L }.toMap
    val finalCounts = polymerization(initialCounts, rules, step).toList
      .flatMap {
        case (Pair(a, b), count) => List(a -> count, b -> count)
      }.foldLeft(Map[Char, Long]()) {
        case (counts, (char, count)) => counts.updatedWith(char)(_.map(_ + count).orElse(Some(count)))
      }.map {
        case char -> count => char -> count / 2
      }.map {
        case char -> count if char == polymer.head || char == polymer.last => count + 1
        case _ -> count => count
      }
    finalCounts.max - finalCounts.min
  }

  override def solve(input: List[String]) = calculate(input, 10)
  override def solveSecondPart(input: List[String]) = calculate(input, 40)
  solve()
}
