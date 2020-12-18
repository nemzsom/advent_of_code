package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.Solver

import scala.annotation.tailrec

object Day_18 extends App with Solver {

  override def solve(input: List[String]) = input.map(Evaluation(0, Add, _).eval().prevValue).sum.toString

  override def solveSecondPart(input: List[String]) = input.map(Node.parse).map(_.eval()).sum.toString

  sealed trait Operator {
    def apply(x: Long, y: Long): Long
  }
  case object Multiply extends Operator {
    override def apply(x: Long, y: Long): Long = x * y
  }
  case object Add extends Operator {
    override def apply(x: Long, y: Long): Long = x + y
  }
  case object NoOp extends Operator {
    override def apply(x: Long, y: Long): Long = throw new IllegalStateException
  }

  object Operator {
    def apply(ch: Char): Operator = ch match {
      case '+' => Add
      case '*' => Multiply
    }
  }

  case class Evaluation(prevValue: Long, prevOp: Operator, expression: String) {

    final def eval(): Evaluation = expression match {
      case "" => this
      case s" $rest" => this.copy(expression = rest).eval()
      case s"($rest" =>
        val eval = Evaluation(0, Add, rest).eval()
        Evaluation(prevOp.apply(prevValue, eval.prevValue), NoOp, eval.expression).eval()
      case s")$rest" => this.copy(expression = rest)
      case exp if exp.head.isDigit => Evaluation(prevOp.apply(prevValue, exp.head.asDigit), NoOp, exp.tail).eval()
      case s"+ $rest" => this.copy(prevOp = Add,      expression = rest).eval()
      case s"* $rest" => this.copy(prevOp = Multiply, expression = rest).eval()
    }
  }

  sealed trait Value
  case class Num(v: Long) extends Value
  case class OpValue(op: Operator) extends Value
  case class Node(left: Node, right: Node, value: Value) {
    def eval(): Long = value match {
      case Num(v) => v
      case OpValue(op) => op.apply(left.eval(), right.eval())
    }
  }

  object Node {

    def apply(value: Long): Node = Node(null, null, Num(value))

    def parse(expression: String): Node = _parse(expression.filter(_ != ' '))

    def _parse(expression: String): Node = parseWithoutParenthesis(evalPars(expression))

    @tailrec
    def evalPars(expression: String): String = {
      val beginParIndex = expression.indexOf('(')
      if (beginParIndex < 0) expression
      else {
        val closeParIndex = findClosingIndex(expression.substring(beginParIndex + 1), 1, beginParIndex + 1)
        val left = expression.substring(0, beginParIndex)
        val right = expression.substring(closeParIndex + 1, expression.length)
        val insidePar =  expression.substring(beginParIndex + 1, closeParIndex)
        evalPars(left + _parse(insidePar).eval() + right)
      }
    }

    @tailrec
    def findClosingIndex(expression: String, openCount: Int, currentIndex: Int): Int = {
      expression.head match {
        case ')' if openCount == 1 => currentIndex
        case ')' => findClosingIndex(expression.tail, openCount - 1, currentIndex + 1)
        case '(' => findClosingIndex(expression.tail, openCount + 1, currentIndex + 1)
        case _ => findClosingIndex(expression.tail, openCount, currentIndex + 1)
      }
    }

    def parseWithoutParenthesis(expression: String): Node = {
      _findMultiply(expression)
        .orElse(_findPlus(expression))
        .getOrElse(Node(expression.toLong))
    }

    def _findPlus(expression: String): Option[Node] = _findOp(expression, '+', OpValue(Add))
    def _findMultiply(expression: String): Option[Node] = _findOp(expression, '*', OpValue(Multiply))

    def _findOp(expression: String, opCh: Char, opCreator: => OpValue): Option[Node] = {
      val i = expression.indexOf(opCh)
      if (i < 0) None
      else {
        val leftE = expression.substring(0, i)
        val rightE = expression.substring(i + 1, expression.length)
        Some(Node(parseWithoutParenthesis(leftE), parseWithoutParenthesis(rightE), opCreator))
      }
    }
  }

  solve()
}
