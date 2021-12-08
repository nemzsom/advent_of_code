package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.Solver

object Day_8 extends App with Solver {

  case class Mapping(order: Array[Char], tenDigits: Array[Digit]) {
    def matches(display: Display): Boolean = tenDigits.forall(display.patterns.contains)
    def decode(display: Display): Option[Int] =
    if (matches(display)) {
      display.digits.map(digit => tenDigits.indexOf(digit)).toList match {
        case x :: y :: z :: w :: Nil => Some(1000*x + 100*y + 10*z + w)
      }
    } else None
  }
  object Mapping {
    def apply(order: Array[Char]): Mapping = Mapping(order, Array(
      Digit(Set(0, 1, 2, 4, 5, 6).map(order)),    // 0
      Digit(Set(2, 5).map(order)),                // 1
      Digit(Set(0, 2, 3, 4, 6).map(order)),       // 2
      Digit(Set(0, 2, 3, 5, 6).map(order)),       // 3
      Digit(Set(1, 2, 3, 5).map(order)),          // 4
      Digit(Set(0, 1, 3, 5, 6).map(order)),       // 5
      Digit(Set(0, 1, 3, 4, 5, 6).map(order)),    // 6
      Digit(Set(0, 2, 5).map(order)),             // 7
      Digit(Set(0, 1, 2, 3, 4, 5, 6).map(order)), // 8
      Digit(Set(0, 1, 2, 3, 5, 6).map(order))     // 9
    ))
    val allPossible: List[Mapping] = Array('a', 'b', 'c', 'd', 'e', 'f', 'g').permutations
      .map(Mapping(_)).toList
  }

  case class Digit(segments: Set[Char])
  case class Display(patterns: Set[Digit], digits: Array[Digit])
  object Display {
    def apply(s: String): Display = s.split("\\|").map(_.trim)
        .map(_.split(" ").map(segments => Digit(segments.toCharArray.toSet))).toList match {
        case patterns :: digits :: Nil => Display(patterns.toSet, digits)
      }
  }

  override def solve(input: List[String]) = input.map(Display(_))
    .flatMap(_.digits)
    .count(d => Set(2, 3, 4, 7).contains(d.segments.size))

  override def solveSecondPart(input: List[String]) = input.map(Display(_))
      .flatMap(display =>
        Mapping.allPossible.find(m => m.matches(display))
          .flatMap(matched => matched.decode(display)))
      .sum

  solve()
}
