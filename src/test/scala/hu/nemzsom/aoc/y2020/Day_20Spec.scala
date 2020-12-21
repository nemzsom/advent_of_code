package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.y2020.Day_20.Tile
import hu.nemzsom.aoc.{Solver, Tester}

class Day_20Spec extends Tester {
  override def solver: Solver = Day_20

  override def input: String =
    """Tile 2311:
      |..##.#..#.
      |##..#.....
      |#...##..#.
      |####.#...#
      |##.##.###.
      |##...#.###
      |.#.#.#..##
      |..#....#..
      |###...#.#.
      |..###..###
      |
      |Tile 1951:
      |#.##...##.
      |#.####...#
      |.....#..##
      |#...######
      |.##.#....#
      |.###.#####
      |###.##.##.
      |.###....#.
      |..#.#..#.#
      |#...##.#..
      |
      |Tile 1171:
      |####...##.
      |#..##.#..#
      |##.#..#.#.
      |.###.####.
      |..###.####
      |.##....##.
      |.#...####.
      |#.##.####.
      |####..#...
      |.....##...
      |
      |Tile 1427:
      |###.##.#..
      |.#..#.##..
      |.#.##.#..#
      |#.#.#.##.#
      |....#...##
      |...##..##.
      |...#.#####
      |.#.####.#.
      |..#..###.#
      |..##.#..#.
      |
      |Tile 1489:
      |##.#.#....
      |..##...#..
      |.##..##...
      |..#...#...
      |#####...#.
      |#..#.#.#.#
      |...#.#.#..
      |##.#...##.
      |..##.##.##
      |###.##.#..
      |
      |Tile 2473:
      |#....####.
      |#..#.##...
      |#.##..#...
      |######.#.#
      |.#...#.#.#
      |.#########
      |.###.#..#.
      |########.#
      |##...##.#.
      |..###.#.#.
      |
      |Tile 2971:
      |..#.#....#
      |#...###...
      |#.#.###...
      |##.##..#..
      |.#####..##
      |.#..####.#
      |#..#.#..#.
      |..####.###
      |..#.#.###.
      |...#.#.#.#
      |
      |Tile 2729:
      |...#.#.#.#
      |####.#....
      |..#.#.....
      |....#..#.#
      |.##..##.#.
      |.#.####...
      |####.#.#..
      |##.####...
      |##..#.##..
      |#.##...##.
      |
      |Tile 3079:
      |#.#.#####.
      |.#..######
      |..#.......
      |######....
      |####.#..#.
      |.#...#.##.
      |#.#####.##
      |..#.###...
      |..#.......
      |..#.###...""".stripMargin

  override val expectedResult: String = "20899048083289"

  override def expectedResultPart2: String = "273"

  "Flip tiles" should "restore original state" in { foreachTile { tile =>
    assert(tile === tile.flip.flip)
  }}

  "Tiles full rotation" should "restore original state" in { foreachTile { tile =>
    assert(tile === tile.rotate180.rotate180)
    assert(tile === tile.rotate270.rotate90)
    assert(tile === tile.rotate90.rotate90.rotate90.rotate90)
  }}

  def foreachTile(assertion: Tile => Unit): Unit = Day_20.parse(asLines(input)).foreach(assertion)
}
