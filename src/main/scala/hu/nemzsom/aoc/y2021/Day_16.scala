package hu.nemzsom.aoc.y2021

import hu.nemzsom.aoc.Solver
import hu.nemzsom.aoc.y2021.Day_16.{Binary, Packet}

import scala.annotation.tailrec

object Day_16 extends App with Solver {

  type Binary = String

  implicit class BinaryFunctions(binary: Binary) {
    def packetVersion: Int = binary.substring(0, 3).asInt
    def packetTypeId: Int = binary.substring(3, 6).asInt
    def lengthTypeId: Int = binary.substring(6, 7).asInt
    def operatorTotalLength: Int = binary.substring(7, 22).asInt
    def operatorSubPacketsLength: Int = binary.substring(7, 18).asInt
    def operatorType0Binary: Binary = binary.substring(22)
    def operatorType1Binary: Binary = binary.substring(18)
    def packetLiteral: Binary = binary.substring(6)

    def asInt: Int = Integer.parseInt(binary, 2)
    def asLong: Long = java.lang.Long.parseLong(binary, 2)
    def extractLiteral: (Long, Binary) = {
      @tailrec
      def extractBinary(acc: Binary, b: Binary): (Binary, Binary) = {
        b.head match {
          case '1' => extractBinary(acc + b.substring(1, 5), b.substring(5))
          case '0' => (acc + b.substring(1, 5), b.substring(5))
        }
      }
      val (literalBinary, tail) = extractBinary("", binary)
      (literalBinary.asLong, tail)
    }
  }

  def hexToBinary(str: String): String = {
    val expectedLength = str.length * 4
    val binary = BigInt(str, 16).toString(2)
    val diff = expectedLength - binary.length
    Array.fill(diff)("0").mkString + binary
  }

  trait Packet {
    def version: Int
    def typeId: Int
    def sumVersions: Int
    def calculate: Long
  }
  case class Literal(version: Int, typeId: Int, value: Long) extends Packet {
    override def sumVersions: Int = version
    override def calculate: Long = value
  }
  case class Operator(version: Int, typeId: Int, subPackages: List[Packet]) extends Packet {
    override def sumVersions: Int = version + subPackages.map(_.sumVersions).sum
    override def calculate: Long = typeId match {
      case 0 => subPackages.map(_.calculate).sum
      case 1 => subPackages.map(_.calculate).product
      case 2 => subPackages.map(_.calculate).min
      case 3 => subPackages.map(_.calculate).max
      case 5 => if (subPackages.head.calculate > subPackages(1).calculate) 1 else 0
      case 6 => if (subPackages.head.calculate < subPackages(1).calculate) 1 else 0
      case 7 => if (subPackages.head.calculate == subPackages(1).calculate) 1 else 0
    }
  }

  def parsePacket(binary: Binary): (Packet, Binary) = {
    val version = binary.packetVersion
    val typeId = binary.packetTypeId
    typeId match {
      case 4 => parseLiteral(version, typeId, binary.packetLiteral)
      case _ if binary.lengthTypeId == 0 => parseOperator(version, typeId, binary.operatorTotalLength, 0, binary.operatorType0Binary)
      case _ => parseOperator(version, typeId, binary.operatorSubPacketsLength, 1, binary.operatorType1Binary)
    }
  }

  @tailrec
  def parseOperator(version: Int, typeId: Int, remainder: Int, binaryLengthId: Int, binary: Binary, acc: List[Packet] = List()): (Packet, Binary) =
    if (remainder == 0) (Operator(version, typeId, acc), binary)
    else {
      val (packet, tail) = parsePacket(binary)
      val nextRemainder = if (binaryLengthId == 0) remainder - (binary.length - tail.length) else remainder - 1
      parseOperator(version, typeId, nextRemainder, binaryLengthId, tail, acc :+ packet)
    }

  def parseLiteral(version: Int, typeId: Int, binary: Binary): (Packet, Binary) = {
    val (literal, tail) = binary.extractLiteral
    (Literal(version, typeId, literal), tail)
  }

  override def solve(input: List[String]) = parsePacket(hexToBinary(input.head))._1.sumVersions
  override def solveSecondPart(input: List[String]) = parsePacket(hexToBinary(input.head))._1.calculate

  solve()
}
