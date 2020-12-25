package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.Solver

import scala.annotation.tailrec

object Day_25 extends App with Solver {

  override def solve(input: List[String]) = {
    val i = Input(input)
    val encryptionKeyC = i.card.calculateEncryptionKey(i.door)
    val encryptionKeyD = i.door.calculateEncryptionKey(i.card)
    if (encryptionKeyC != encryptionKeyD)
      throw new AssertionError(s"Card's encryption key: $encryptionKeyC, Door's encryption key: $encryptionKeyD")
    encryptionKeyC.toString
  }

  override def solveSecondPart(input: List[String]) = ???

  case class Encryption(publicKey: Long, subject: Long) {
    import Encryption._

    val loopSize: Int = {
      @tailrec
      def findLoopSize(value: Long = 1, round: Int = 0): Int = {
        if (value == publicKey) round
        else findLoopSize(transform(subject, value), round + 1)
      }
      findLoopSize()
    }

    def calculateEncryptionKey(encryption: Encryption): Long = {
      println(s"calculate enc key for $encryption")
      loop(encryption.publicKey, loopSize)
    }
  }

  object Encryption {
    val divisor = 20201227

    def transform(subject: Long, value: Long = 1): Long = (value * subject) % divisor

    @tailrec
    def loop(subject: Long, loopSize: Int, value: Long = 1): Long =
      if (loopSize <= 0) value
      else loop(subject, loopSize - 1, transform(subject, value))
  }

  case class Input(card: Encryption, door: Encryption)
  object Input {
    def apply(line: List[String]): Input = Input(
      Encryption(line.head.toInt, 7),
      Encryption(line(1).toInt, 7)
    )
  }

  solve()
}
