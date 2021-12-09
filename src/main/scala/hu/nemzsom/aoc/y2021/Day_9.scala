package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.Solver
import hu.nemzsom.aoc.y2021.Day_9.Point.p

object Day_9 extends App with Solver {

  type HeightMap = Array[Array[Int]]
  type Basin = Set[Point]

  case class Point(x: Int, y: Int) {
    lazy val adjacent: List[Point] = List(p(x + 1, y), p(x - 1, y), p(x, y + 1), p(x, y - 1))
    def withinMap(heightMap: HeightMap): Boolean =
      y >= 0 && y < heightMap.length && x >= 0 && x < heightMap(y).length
  }
  object Point {
    def p(x: Int, y: Int): Point = Point(x, y)
  }

  def height(map: HeightMap, p: Point): Int = map(p.y)(p.x)
  def isLowest(map: HeightMap, p: Point): Boolean = {
    val h = height(map, p)
    p.adjacent.filter(_.withinMap(map)).map(height(map, _)).forall(_ > h)
  }
  def riskLevel(height: Int) = height + 1

  def parse(input: List[String]): HeightMap = input.map(_.toCharArray.map(_.asDigit)).toArray

  def findLowestPoints(map: HeightMap): Seq[Point] =
    for (y <- map.indices;
         x <- map(0).indices;
         point = p(x, y) if isLowest(map, point)
    ) yield point

  def findBasin(map: HeightMap, from: Point, found: Basin): Basin = {
    val nextPoints = from.adjacent.filter(p => p.withinMap(map) && height(map, p) < 9 && !found.contains(p))
    nextPoints.foldLeft(found ++ nextPoints)((foundUpdated, nextPoint) => findBasin(map, nextPoint, foundUpdated))
  }

  override def solve(input: List[String]) = {
    val map = parse(input)
    val lowestPoints = findLowestPoints(map)
    lowestPoints.map(p => height(map, p)).map(riskLevel).sum
  }

  override def solveSecondPart(input: List[String]) = {
    val map = parse(input)
    val basins = findLowestPoints(map).map(p => findBasin(map, p, Set(p)))
    basins.map(_.size).takeRight(3).product
  }

  solve()
}
