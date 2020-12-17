package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.Solver

import scala.util.matching.Regex

object Day_16 extends App with Solver {

  override def solve(input: List[String]) = {
    val Input(fields, _, otherTickets) = Input(input)
    otherTickets.flatMap(ticket =>
      ticket.numbers.filter(x =>
        fields.forall(field => !field.validation.isValid(x)
        )
      )
    ).sum.toString
  }

  override def solveSecondPart(input: List[String]) = {
    val i = Input(input)
    val fields = findPositions(i)
    fields.sorted.map(_.name).zip(i.myTicket.numbers)
      .filter{ case (name, _) => name.startsWith("departure")}
      .map(_._2.toLong)
      .product
      .toString
  }

  def findPositions(input: Input): List[Field] = {
    val Input(fields, _, otherTickets) = input
    val validTickets =
      otherTickets.filter(ticket =>
        ticket.numbers.forall(
          x => fields.exists(_.validation.isValid(x))
        )
      )

    def possibleMapping(fields: List[Field]): Map[Field, List[Int]] = {
      fields.map { field =>
        val possiblePositions = fields.indices.filter(i => validTickets.forall(ticket => field.validation.isValid(ticket.numbers(i)))).toList
        (field, possiblePositions)
      }.toMap
    }

    def find(found: List[Field], possibleMappings: Map[Field, List[Int]]): List[Field] =
      if (possibleMappings.isEmpty) found
      else {
        val (field, pos :: Nil) = possibleMappings.filter(_._2.size == 1).head
        find(field.copy(position = pos) :: found, possibleMappings.removed(field).map { case (field, positions) => (field, positions.filter(_ != pos)) })
      }

    find(List(), possibleMapping(fields))
  }

  case class Validation(left: Range, right: Range) {
    def isValid(x: Int): Boolean = left.contains(x) || right.contains(x)
  }
  case class Field(name: String, validation: Validation, position: Int) extends Ordered[Field] {
    override def compare(that: Field): Int = position.compareTo(that.position)
  }
  case class Ticket(numbers: List[Int])
  case class Input(fields: List[Field], myTicket: Ticket, otherTickets: List[Ticket])
  
  object Field {
    val pattern: Regex = "(.+): (\\d+)-(\\d+) or (\\d+)-(\\d+)".r

    def apply(str: String): Field = {
      val pattern(name, leftFrom, leftTo, rightFrom, rightTo) = str
      Field(name, Validation(range(leftFrom, leftTo), range(rightFrom, rightTo)), -1)
    }

    def range(from: String, to: String) = Range.inclusive(from.toInt, to.toInt)
  }

  object Input {
    def apply(lines: List[String]): Input = {
      val (fields, tickets) =  lines.span(!_.isBlank)
      val myTickets = tickets.drop(2).head.split(",").map(_.toInt).toList
      val otherTickets = tickets.drop(5).map(line => line.split(",").map(_.toInt).toList)
      Input(fields.map(Field(_)), Ticket(myTickets), otherTickets.map(Ticket))
    }
  }

  solve()
}
