package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.Solver

object Day_18 extends App with Solver {

  case class Explosion(newNumber: SFNumber, leftAdd: Option[Int], rightAdd: Option[Int])

  trait SFNumber {
    def regularValue: Int
    def explode: Option[SFNumber] = explode(0).map(_.newNumber)
    def explode(depth: Int): Option[Explosion]
    def split: Option[SFNumber]
    def magnitude: Int
    def reduce: SFNumber = explode.orElse(split).map(_.reduce).getOrElse(this)
    def +(other: SFNumber): SFNumber = SFPair(this, other).reduce
    def addLeft(n: Int): SFNumber
    def addRight(n: Int): SFNumber
  }
  case class SFRegular(n: Int) extends SFNumber {
    override def regularValue: Int = n
    override def magnitude: Int = n
    override def toString: String = n.toString
    override def explode(depth: Int): Option[Explosion] = None
    override def split: Option[SFNumber] =
      if (n < 10) None
      else Some(SFPair(SFRegular(n / 2), SFRegular(Math.ceil(n / 2.0).intValue)))

    override def addLeft(addition: Int): SFNumber = SFRegular(addition + n)
    override def addRight(addition: Int): SFNumber = SFRegular(addition + n)
  }
  case class SFPair(left: SFNumber, right: SFNumber) extends SFNumber {
    override def regularValue: Int = throw new IllegalStateException("SFPair does not have a regular value")
    override def toString: String = s"[$left,$right]"
    override def explode(depth: Int): Option[Explosion] =
      if (depth < 4 || left.isInstanceOf[SFPair] || right.isInstanceOf[SFPair]) {
        left.explode(depth + 1).map { case Explosion(newNumber, leftRemainder, rightRemainder) =>
          Explosion(SFPair(newNumber, rightRemainder.map(rr => right.addLeft(rr)).getOrElse(right)), leftRemainder, None)
        }.orElse(right.explode(depth + 1).map { case Explosion(newNumber, leftRemainder, rightRemainder) =>
          Explosion(SFPair(leftRemainder.map(lr => left.addRight(lr)).getOrElse(left), newNumber), None, rightRemainder)
        })
      } else {
        Some(Explosion(SFRegular(0), Some(left.regularValue), Some(right.regularValue)))
      }

    override def split: Option[SFNumber] =
      left.split.map(SFPair(_, right))
        .orElse(right.split.map(SFPair(left, _)))

    override def magnitude: Int = left.magnitude * 3 + right.magnitude * 2

    override def addLeft(n: Int): SFNumber = SFPair(left.addLeft(n), right)
    override def addRight(n: Int): SFNumber = SFPair(left, right.addRight(n))
  }

  object SFNumber {
    def apply(literal: String): SFNumber = parse(literal.toList)._1
    private def parse(chars: List[Char]): (SFNumber, List[Char]) = chars match {
      case d10 :: d1 :: tail if d10.isDigit && d1.isDigit =>
        (SFRegular(10 * d10.asDigit + d1.asDigit), tail) // allows two digit numbers
      case d :: tail if d.isDigit => (SFRegular(d.asDigit), tail)
      case '[' :: tail =>
        val (left, ',' :: rightRemained) = parse(tail)
        val (right, ']' :: remained) = parse(rightRemained)
        (SFPair(left, right), remained)
    }
  }

  override def solve(input: List[String]) = input.map(SFNumber(_)).reduce((a, b) => a + b).magnitude
  override def solveSecondPart(input: List[String]) = {
    val xs = input.map(SFNumber(_))
    (for {
      x <- xs
      y <- xs if x != y
    } yield x + y).map(_.magnitude).max
  }

  solve()
}
