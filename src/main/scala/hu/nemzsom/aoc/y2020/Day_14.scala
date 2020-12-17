package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.Solver

import scala.util.matching.Regex

object Day_14 extends App with Solver {

  override def solve(input: List[String]) =
    input.map(toInstruction)
      .foldLeft(State(Map(), Mask("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")))((state, instruction) =>
        state.execute(instruction)
      ).sum.toString

  override def solveSecondPart(input: List[String]) =
    input.map(toInstruction)
      .foldLeft(State(Map(), Mask("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")))((state, instruction) =>
        state.execute2(instruction)
      ).sum.toString

  def toInstruction(str: String): Instruction =
    if (Write.pattern.matches(str)) {
      val Write.pattern(address, value) = str
      Write(address.toLong, value.toLong)
    } else {
      val Mask.pattern(mask) = str
      Mask(mask)
    }

  sealed trait Instruction
  case class Mask(mask: List[Char]) extends Instruction {
    def apply(x: Long): Long = {
      x.to36bitStr.zip(mask).map {
        case (_, '0') => '0'
        case (_, '1') => '1'
        case (bit, _) => bit
      }.mkString("").from36bitStr
    }

    def apply2(x: Long): List[Long] = {
      val floatingAddress = x.to36bitStr.zip(mask).map {
        case (bit, '0') => bit
        case (_, '1') => '1'
        case (_, 'X') => 'X'
      }.mkString("")
      def generateAddresses(floating: String): List[String] = {
        val i = floating.indexOf('X')
        if (i == -1) List(floating)
        else generateAddresses(floating.replaceFirst("X", "0")) ++ generateAddresses(floating.replaceFirst("X", "1"))
      }
      generateAddresses(floatingAddress).map(_.from36bitStr)
    }
  }
  case class Write(address: Long, value: Long) extends Instruction
  case class State(mem: Map[Long, Long], mask: Mask) {
    def execute(instruction: Instruction): State = instruction match {
      case m@Mask(_) => this.copy(mask = m)
      case Write(address, value) => this.copy(mem = mem.updated(address, mask.apply(value)))
    }
    def execute2(instruction: Instruction): State = instruction match {
      case m@Mask(_) => this.copy(mask = m)
      case Write(address, value) => {
        val updatedMem = mask.apply2(address).foldLeft(mem)((m, address) => m.updated(address, value))
        this.copy(mem = updatedMem)
      }
    }
    def sum: Long = mem.values.sum
  }

  object Write {
    val pattern: Regex = "mem\\[(\\d+)] = (.+)".r
  }

  object Mask {
    val pattern: Regex = "mask = (.+)".r
    def apply(maskStr: String): Mask = Mask(maskStr.toList)
  }

  implicit class LongToStr(l: Long) {
    def to36bitStr: String = l.toBinaryString.reverse.padTo(36, '0').reverse
  }
  implicit class StrToLong(s: String) {
    def from36bitStr: Long = java.lang.Long.parseLong(s, 2)
  }

  solve()
}
