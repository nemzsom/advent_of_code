package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.{Solver, Tester}

class Day_21Spec extends Tester {
  override def solver: Solver = Day_21

  override def input: String =
    """mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
      |trh fvjkl sbzzf mxmxvkd (contains dairy)
      |sqjhc fvjkl (contains soy)
      |sqjhc mxmxvkd sbzzf (contains fish)""".stripMargin

  override val expectedResult: String = "5"

  override def expectedResultPart2: String = "mxmxvkd,sqjhc,fvjkl"
}
