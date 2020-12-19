package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.Solver

object Day_19 extends App with Solver {

  override def solve(i: List[String]) = solveByGeneratingAVeryBigRegex(i)

  override def solveSecondPart(i: List[String]) = solveByGeneratingAVeryBigRegex(i, part2 = true)

  def solveByGeneratingAVeryBigRegex(i: List[String], part2: Boolean = false) = {
    val input = Input(i)
    val pattern = input.rules(0).resolvePattern(input.rules, part2)
    input.messages.count(pattern.r.matches).toString
  }

  case class Input(rules: Map[Int, Rule], messages: List[String])
  object Input {
    def apply(input: List[String]): Input = {
      val (specs, messages) = input.span(_.nonEmpty)
      val ruleSpecs = specs
        .map(_.split(":"))
        .map { spec =>
          val id = spec(0).toInt
          (id, Rule(id, spec(1).trim))
        }
        .toMap
      Input(ruleSpecs, messages.tail)
    }
  }

  case class Rule(id: Int, spec: String) {

    def resolvePattern(rules: Map[Int, Rule], part2: Boolean = false): String = {
      val p = spec.split(' ').foldLeft("") {
        case (acc, ch) if ch.contains("\"") => acc + ch.tail.head
        case (acc, "|") => acc + "|"
        case (acc, x) =>
          val pattern = rules(x.toInt).resolvePattern(rules, part2)
          if (part2 && id == 11) acc + pattern + "{X}"
          else acc + pattern
      }
      val res =
        if (part2 && id == 8) p + "+"
        else if (part2 && id == 11)
          '(' + Range(1, 10).map(repeat => p.replaceAll("X", repeat.toString)).mkString("|") + ')'
        else if (spec.contains("|")) '(' + p + ')'
        else p
      res
    }

  }

  solve()
}
