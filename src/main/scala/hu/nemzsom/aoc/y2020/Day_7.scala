package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.Solver

object Day_7 extends App with Solver {

  import Const._

  override def solve(input: List[String]) = {
    val bags = getBags(input)
    def searchFor(bagName: String): List[String] = {
      bags.filter { case (_, contents) => contents.map(_.name).contains(bagName) }
        .flatMap { case (name, _) => name :: searchFor(name)}
        .toList
    }
    searchFor("shiny gold").toSet.size.toString
  }

  def getBags(input: List[String]): Map[String, List[Content]] =
  input.map(line => {
    (outerBagPattern.findFirstMatchIn(line).get.group(1),
      contentPattern.findAllIn(line).matchData
        .map(m => Content(m.group(2), m.group(1).toInt)).toList)
  }).toMap

  override def solveSecondPart(input: List[String]) = {
    val bags = getBags(input)
    def count(bagName: String): Int = bags(bagName).map(content => {
      //println(s"found ${content} and add ${content.count} + ${content.count} * ${count(content.name)}")
      content.count + content.count * count(content.name)
    }).sum
    count("shiny gold").toString
  }

  solve()
}

case class Content(name: String, count: Int)

object Const {
  val outerBagPattern = "(.+) bags contain".r
  val contentPattern = "(\\d+) (.+?) bag".r
}
