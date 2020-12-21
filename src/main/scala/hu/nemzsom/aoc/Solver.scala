package hu.nemzsom.aoc

import java.time.Duration
import java.time.temporal.ChronoUnit

import scala.io.Source
import scala.util.Try

trait Solver {

  def solve(input: List[String]): String
  def solveSecondPart(input: List[String]): String

  def solve(): Unit = {
    val start1 = System.nanoTime()
    val first = Try(solve(getInput))
    first.foreach(res => {
      val duration = Duration.of(System.nanoTime() - start1, ChronoUnit.NANOS)
      println(s"Result first part ($duration):")
      println(res)
    })
    first.failed.foreach(th => {
      println(s"Part 1 failed: ${th.getMessage}")
      th.printStackTrace()
    })
    val start2 = System.nanoTime()
    val second = Try(solveSecondPart(getInput))
    second.foreach(res => {
      val duration = Duration.of(System.nanoTime() - start2, ChronoUnit.NANOS)
      println(s"Result second part ($duration):")
      println(res)
    })
    second.failed.foreach(th => {
      println(s"Part 2 failed: ${th.getMessage}")
      th.printStackTrace()
    })
  }

  def getInput: List[String] = {
    val className = getClass.getSimpleName
    val day  = "Day_(\\d+).+".r.findFirstMatchIn(className).head.group(1)
    Source.fromInputStream(getClass.getResourceAsStream(s"input_$day.txt")).getLines().toList
  }
}
