package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.Solver


object Day_10 extends App with Solver {

  sealed trait BracketType {
    def open: Char
    def close: Char
    def point1: Int
    def point2: Int
  }
  case object Parentheses extends BracketType { val open = '('; val close = ')'; val point1 = 3; val point2 = 1 }
  case object SquareBrackets extends BracketType { val open = '['; val close = ']'; val point1 = 57; val point2 = 2 }
  case object CurlyBrackets extends BracketType { val open = '{'; val close = '}'; val point1 = 1197; val point2 = 3 }
  case object AngleBrackets extends BracketType { val open = '<'; val close = '>'; val point1 = 25137; val point2 = 4 }

  case class Bracket(t: BracketType, open: Boolean)
  object Bracket {
    val allTypes = Set(Parentheses, SquareBrackets, CurlyBrackets, AngleBrackets)
    def apply(char: Char): Bracket = allTypes.flatMap(t =>
      if (t.open == char) Some(Bracket(t, open = true))
      else if (t.close == char) Some(Bracket(t, open = false))
      else None
    ).head
  }

  case class Validator(opened: List[BracketType]) {
    def next(bracket: Bracket): Either[BracketType, Validator] = {
      if (bracket.open) Right(Validator(bracket.t :: opened))
      else if (bracket.t == opened.head) Right(Validator(opened.tail))
      else Left(bracket.t)
    }
  }

  def parse(input: List[String]): List[List[Bracket]] = input.map(_.toCharArray.map(Bracket(_)).toList)

  def validate(line: List[Bracket], validator: Validator = Validator(List())): Either[BracketType, Validator] = line match {
    case Nil => Right(validator)
    case head :: tail => validator.next(head).flatMap(validate(tail, _))
  }

  override def solve(input: List[String]) = parse(input).map(validate(_)).filter(_.isLeft).map {
    case Left(t) => t.point1
  }.sum

  override def solveSecondPart(input: List[String]) = {
    val points = parse(input).map(validate(_)).filter(_.isRight).map {
      case Right(Validator(opened)) => opened.foldLeft(0L)((sum, open) => sum * 5 + open.point2)
    }.sorted
    points(points.size / 2)
  }

  solve()
}
