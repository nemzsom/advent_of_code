package hu.nemzsom.aoc

import scala.io.Source
import scala.util.Try

trait Solver {

  def solve(input: List[String]): String
  def solveSecondPart(input: List[String]): String

  def solve(): Unit = {
    val first = Try(solve(getInput))
    first.foreach(res => {
      println("Result first part:")
      println(res)
    })
    first.failed.foreach(th => {
      println(s"Part 1 failed: ${th.getMessage}")
      th.printStackTrace()
    })
    val second = Try(solveSecondPart(getInput))
    second.foreach(res => {
      println("Result second part:")
      println(res)
    })
    second.failed.foreach(th => {
      println(s"Part 2 failed: ${th.getMessage}")
      th.printStackTrace()
    })
  }

  def getInput = {
    val className = getClass.getSimpleName
    val day  = "Day_(\\d+).+".r.findFirstMatchIn(className).head.group(1)
    Source.fromInputStream(getClass.getResourceAsStream(s"input_$day.txt")).getLines().toList
  }
}
