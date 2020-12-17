package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.Solver

object Day_8 extends App with Solver {

  override def solve(input: List[String]) = {
    val instructions = input.map(Op(_)).toVector
    execute(Acc(0, 0, instructions, infiniteLoop = false)).value.toString
  }

  override def solveSecondPart(input: List[String]) = {
    val instructions = input.map(Op(_)).toVector
    instructions.indices
      .map(i => {
        val op = instructions(i)
        op.op match {
          case "jmp" => Some(execute(Acc(0, 0, instructions.updated(i, op.copy(op = "nop")), infiniteLoop = false)))
          case "nop" => Some(execute(Acc(0, 0, instructions.updated(i, op.copy(op = "jmp")), infiniteLoop = false)))
          case _ => None
        }
      }).filter(_.isDefined)
      .map(_.get)
      .find(!_.infiniteLoop)
      .get.value.toString
  }

  def execute(acc: Acc): Acc = {
    if (acc.instructions.size == acc.index) acc
    else {
      val nextOp = acc.instructions(acc.index)
      if (nextOp.executed) acc.copy(infiniteLoop = true)
      else execute(nextOp.execute(acc))
    }
  }

  case class Op(op: String, value: Int, executed: Boolean) {
    def execute(acc: Acc): Acc = {
      val updatedInstructions = acc.instructions.updated(acc.index, this.copy(executed = true))
      op match {
        case "acc" => acc.copy(value = acc.value + value, index = acc.index + 1, instructions = updatedInstructions)
        case "jmp" => acc.copy(index = acc.index + value, instructions = updatedInstructions)
        case "nop" => acc.copy(index = acc.index + 1, instructions = updatedInstructions)
      }
    }
  }

  case class Acc(value: Int, index: Int, instructions: Vector[Op], infiniteLoop: Boolean)

  object Op {
    def apply(spec: String): Op = spec.split(' ').toList match {
      case op :: value :: Nil => Op(op, value.toInt, executed = false)
    }
  }

  solve()
}
