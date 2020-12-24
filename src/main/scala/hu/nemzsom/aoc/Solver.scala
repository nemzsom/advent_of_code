package hu.nemzsom.aoc

import java.time.Duration
import java.time.temporal.ChronoUnit

import scala.io.Source
import scala.util.Try

trait Solver {

  val newLine: String = System.lineSeparator()

  def solve(input: List[String]): String
  def solveSecondPart(input: List[String]): String

  def solve(): Unit = {
    solve("Part 1", solve)
    solve("Part 2", solveSecondPart)
  }

  private def solve(desc: String, solver: List[String] => String): Unit = {
    val start = System.nanoTime()
    val result = Try(solver(getInput))
    result.foreach(res => {
      val duration = Duration.of(System.nanoTime() - start, ChronoUnit.NANOS)
      println(s"$desc ($duration):")
      println(res)
    })
    result.failed.foreach(th => {
      println(s"$desc failed: ${th.getMessage}")
      if (!th.isInstanceOf[NotImplementedError]) th.printStackTrace()
    })
  }

  def getInput: List[String] = {
    val className = getClass.getSimpleName
    val day  = "Day_(\\d+).+".r.findFirstMatchIn(className).head.group(1)
    Source.fromInputStream(getClass.getResourceAsStream(s"input_$day.txt")).getLines().toList
  }
}
