package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.{Solver, Tester}

class Day_3Spec extends Tester {
  override def solver: Solver = Day_3

  override def input: String =
    """..##.......
      |#...#...#..
      |.#....#..#.
      |..#.#...#.#
      |.#...##..#.
      |..#.##.....
      |.#.#.#....#
      |.#........#
      |#.##...#...
      |#...##....#
      |.#..#...#.#""".stripMargin

  override val expectedResult: String = "7"
  override def expectedResultPart2: String = "336"
}
