package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.Solver

object Day_8 extends App with Solver {

  case class Mapping(tenDigits: Array[Digit]) {
    def matches(display: Display): Boolean = tenDigits.forall(display.patterns.contains)
    def decode(display: Display): Int = display.digits.map(digit => tenDigits.indexOf(digit)).toList match {
        case x :: y :: z :: w :: Nil => 1000*x + 100*y + 10*z + w
      }
  }
  object Mapping {
    def apply(order: Array[Char]): Mapping = Mapping(Array(
      Set(0, 1, 2, 4, 5, 6),    // 0
      Set(2, 5),                // 1          0000
      Set(0, 2, 3, 4, 6),       // 2         1    2
      Set(0, 2, 3, 5, 6),       // 3         1    2
      Set(1, 2, 3, 5),          // 4          3333
      Set(0, 1, 3, 5, 6),       // 5         4    5
      Set(0, 1, 3, 4, 5, 6),    // 6         4    5
      Set(0, 2, 5),             // 7          6666
      Set(0, 1, 2, 3, 4, 5, 6), // 8
      Set(0, 1, 2, 3, 5, 6)     // 9
    ).map(segments => Digit(segments.map(order))))
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
          .map(matched => matched.decode(display)))
      .sum

  solve()
}
