package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.Solver

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.matching.Regex

object Day_12 extends App with Solver {

  type Cave = String
  type Path = List[Cave]

  implicit class CaveFunctions(cave: Cave) {
    def isSmall: Boolean = cave.head.isLower
  }

  case class CaveSystem(connections: Map[Cave, List[Cave]]) {

    final def findPathsToEnd(pathSoFar: Path = List("start"))(nextCaveFilter: (Path, Cave) => Boolean): List[Path] = pathSoFar.head match {
      case "end" => List(pathSoFar)
      case actual => connections(actual)
        .filter(next => nextCaveFilter(pathSoFar, next))
        .flatMap(next => findPathsToEnd(next :: pathSoFar)(nextCaveFilter))
    }

    def registerConnection(from: Cave, to: Cave): CaveSystem = CaveSystem(
      connections.updatedWith(from){
        case None => Some(List(to))
        case Some(conns) if conns.contains(from) => Some(conns)
        case Some(conns) => Some(to :: conns)
      }
    )
  }

  object CaveSystem {
    val conn: Regex = """(\w+)-(\w+)""".r
    def parse(input: List[String]): CaveSystem =
      input.foldLeft(CaveSystem(Map()))((caves, line) => line match {
        case conn(from, to) => caves.registerConnection(from, to).registerConnection(to, from)
      })
  }

  def smallCavesOnce(path: Path, next: Cave): Boolean = !path.filter(_.isSmall).contains(next)
  def oneSmallCaveTwice(path: Path, next: Cave): Boolean = {
    val smallCaves = path.filter(_.isSmall)
    next != "start" && (!smallCaves.contains(next) || smallCaves.size == smallCaves.toSet.size)
  }

  override def solve(input: List[String]) = CaveSystem.parse(input).findPathsToEnd()(smallCavesOnce).size
  override def solveSecondPart(input: List[String]) = CaveSystem.parse(input).findPathsToEnd()(oneSmallCaveTwice).size
  solve()
}
