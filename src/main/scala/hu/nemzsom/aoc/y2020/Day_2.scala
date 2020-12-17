package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.Solver

object Day_2 extends App with Solver {

  override def solve(input: List[String]) = {
    input
      .map(Pass(_))
      .count(_.isValid())
      .toString
  }

  override def solveSecondPart(input: List[String]) =
    input
      .map(Pass2(_))
      .count(_.isValid())
      .toString

  object Pass {
    val pattern = "(\\d+)-(\\d+) (.): (.+)".r

    def apply(str: String): Pass = {
      val pattern(min, max, letter, pass) = str
      Pass(letter.head, min.toInt, max.toInt, pass)
    }
  }

  case class Pass(letter: Char, min: Int, max: Int, pass: String) {

    def isValid() = {
      val count = pass.count(_ == letter)
      count >= min && count <= max
    }
  }

  object Pass2 {

    import Pass.pattern

    def apply(str: String): Pass2 = {
      val pattern(pos1, pos2, letter, pass) = str
      Pass2(letter.head, pos1.toInt, pos2.toInt, pass)
    }
  }

  case class Pass2(letter: Char, pos1: Int, pos2: Int, pass: String) {

    def isValid() = {
      List(pass.charAt(pos1 - 1), pass.charAt(pos2 - 1))
        .count(_ == letter) == 1
    }
  }

  solve()
}
