package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.y2021.Day_16.{Literal, Operator, Packet}
import hu.nemzsom.aoc.{Solver, Tester}

class Day_16Spec extends Tester {
  override def solver: Solver = Day_16

  override def input: String = "8A004A801A8002F478"

  override val expectedResult: String = "16"
  override def expectedResultPart2: String = ???

  "The converter" should "convert hex values to binary" in {
    assert(Day_16.hexToBinary("D2FE28") === "110100101111111000101000")
    assert(Day_16.hexToBinary("38006F45291200") === "00111000000000000110111101000101001010010001001000000000")
    assert(Day_16.hexToBinary("EE00D40C823060") === "11101110000000001101010000001100100000100011000001100000")
  }

  "The parser" should "parse literal packages" in {
    assert(Day_16.parsePacket(Day_16.hexToBinary("D2FE28"))._1 === Literal(6, 4, 2021))
  }

  "The parser" should "parse operator packages" in {
    assert(Day_16.parsePacket(Day_16.hexToBinary("38006F45291200"))._1 === Operator(1, 6, List(
      Literal(6, 4, 10), Literal(2, 4, 20)
    )))
    assert(Day_16.parsePacket(Day_16.hexToBinary("EE00D40C823060"))._1 === Operator(7, 3, List(
      Literal(2, 4, 1), Literal(4, 4, 2), Literal(1, 4, 3)
    )))
  }

  "The solution for part1" should "match all examples" in {
    assert(solver.solve(asLines("620080001611562C8802118E34")) === 12)
    assert(solver.solve(asLines("C0015000016115A2E0802F182340")) === 23)
    assert(solver.solve(asLines("A0016C880162017C3686B18A3D4780")) === 31)
  }

  "The solution for part2" should "match all examples" in {
    assert(solver.solveSecondPart(asLines("C200B40A82")) === 3)
    assert(solver.solveSecondPart(asLines("04005AC33890")) === 54)
    assert(solver.solveSecondPart(asLines("880086C3E88112")) === 7)
    assert(solver.solveSecondPart(asLines("CE00C43D881120")) === 9)
    assert(solver.solveSecondPart(asLines("D8005AC2A8F0")) === 1)
    assert(solver.solveSecondPart(asLines("F600BC2D8F")) === 0)
    assert(solver.solveSecondPart(asLines("9C005AC2F8F0")) === 0)
    assert(solver.solveSecondPart(asLines("9C0141080250320F1802104A08")) === 1)
  }
}
