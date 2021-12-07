package hu.nemzsom.aoc

trait Parser {

  def parseFirstLineAsInts(input: List[String]): List[Int] = input.head.split(",").map(_.toInt).toList
  def parseToInts(input: List[String]): List[Int] = input.map(_.toInt)
  def parseToLongs(input: List[String]): List[Long] = input.map(_.toLong)

}
