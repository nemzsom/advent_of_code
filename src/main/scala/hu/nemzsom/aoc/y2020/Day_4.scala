package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.Solver

import scala.util.matching.Regex

object Day_4 extends App with Solver {

  import Field._

  override def solve(input: List[String]) =
    getPassPorts(input)
      .map(line => pattern.findAllIn(line).matchData.map(m => m.group(1)).toSet)
      .count(fields => required.forall(req => fields.contains(req)))
      .toString

  override def solveSecondPart(input: List[String]) =
    getPassPorts(input)
      .map(line => pattern.findAllIn(line).matchData.map(m => Field(m.group(1), m.group(2))).toSet.filter(_.isValid))
      .count(fields => fields.size == 7)
      .toString

  private def getPassPorts(input: List[String]) = {
    input.foldRight(List(""))((line, acc) =>
      if (line.isBlank) "" :: acc
      else acc.updated(0, acc.head + " " + line)
    ).filter(_.nonEmpty)
  }

  solve()
}

object Field {
  val pattern: Regex = "(\\S+):(\\S+)".r
  val required = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

  def apply(key: String, value: String): Field = {
    key match {
      case "byr" => Byr(value)
      case "iyr" => Iyr(value)
      case "eyr" => Eyr(value)
      case "hgt" => Hgt(value)
      case "hcl" => Hcl(value)
      case "ecl" => Ecl(value)
      case "pid" => Pid(value)
      case _ => Unknown(value)
    }
  }
}

trait Field {
  def value: String
  def pattern: Regex
  def isValid: Boolean = pattern.matches(value)
}

case class Byr(value: String) extends Field {
  val pattern: Regex = "\\d{4}".r
  override def isValid = super.isValid && value.toInt >= 1920 && value.toInt <= 2002
}
case class Iyr(value: String) extends Field {
  val pattern: Regex = "\\d{4}".r
  override def isValid = super.isValid && value.toInt >= 2010 && value.toInt <= 2020
}
case class Eyr(value: String) extends Field {
  val pattern: Regex = "\\d{4}".r
  override def isValid = super.isValid && value.toInt >= 2020 && value.toInt <= 2030
}
case class Hgt(value: String) extends Field {
  val pattern: Regex = "(\\d+)(in|cm)".r
  override def isValid = super.isValid && {
    val pattern(hgtStr, metric) = value
    val hgt = hgtStr.toInt
    metric match {
      case "in" => hgt >= 59 && hgt <= 76
      case "cm" => hgt >= 150 && hgt <= 193
    }
  }
}
case class Hcl(value: String) extends Field {
  val pattern: Regex = "#([0-9]|[a-f]){6}".r
}
case class Ecl(value: String) extends Field {
  val pattern: Regex = "amb|blu|brn|gry|grn|hzl|oth".r
}
case class Pid(value: String) extends Field {
  val pattern: Regex = "\\d{9}".r
}
case class Unknown(value: String) extends Field {
  val pattern: Regex = "NA".r
  override def isValid = false
}


