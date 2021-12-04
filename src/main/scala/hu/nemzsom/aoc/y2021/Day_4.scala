package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.Solver

object Day_4 extends App with Solver {

  case class Number(value: Int, marked: Boolean) {
    def mark: Number = Number(value, marked = true)
  }

  case class Board(rows: List[List[Number]]) {
    def sumOfUnmarked: Int = rows.map(_.filter(!_.marked).map(_.value).sum).sum
    def mark(x: Int): Board = Board(rows.map(row =>
      row.map(n => if (n.value == x) n.mark else n)
    ))

    def columns: List[List[Number]] = (for {
      i <- Range(0, 5)
      row <- rows
    } yield row(i)).toList.grouped(5).toList

    def won: Boolean = hasWinningRow || hasWinningColumn
    def hasWinningRow: Boolean = hasWin(rows)
    def hasWinningColumn: Boolean = hasWin(columns)
    private def hasWin(numbers: List[List[Number]]): Boolean = numbers.exists(_.forall(_.marked))
  }
  object Board {
    def parse(literal: List[String]): Board = Board(
      literal.map(_.split("""\s+""").toList
        .filter(_.nonEmpty)
        .map(value => Number(value.toInt, marked = false))))
  }

  case class Result(won: Board, lastNumber: Int) {
    def score: Int = won.sumOfUnmarked * lastNumber
  }

  case class Bingo(drawSequence: List[Int], boards: List[Board]) {

    final def playForWin: Result = drawSequence match {
      case drawn :: tail =>
        val bingo = Bingo(tail, next(drawn))
        bingo.winingBoard.map(Result(_, drawn)).getOrElse(bingo.playForWin)
    }

    def playForLoose: Result = drawSequence match {
      case drawn :: tail => next(drawn) match {
        case last :: Nil if last.won => Result(last, drawn)
        case next => Bingo(tail, next.filter(!_.won)).playForLoose
      }
    }

    private def next(drawn: Int): List[Board] = boards.map(_.mark(drawn))
    private def winingBoard: Option[Board] = boards.find(_.won)
  }

  def parse(input: List[String]): Bingo = {
    val drawSequence = input.head.split(",").map(_.toInt).toList
    val boards = input.tail.filter(_.nonEmpty).grouped(5).map(Board.parse).toList
    Bingo(drawSequence, boards)
  }

  override def solve(input: List[String]) = parse(input).playForWin.score
  override def solveSecondPart(input: List[String]) = parse(input).playForLoose.score
  solve()
}
