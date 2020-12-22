package hu.nemzsom.aoc.y2020

import hu.nemzsom.aoc.Solver

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.Try

object Day_22 extends App with Solver {

  override def solve(input: List[String]) = Game.play(Game(input)).winningPlayer.score.toString

  override def solveSecondPart(input: List[String]) = Game.recursivePlay(Game(input)).winningPlayer.score.toString

  case class Player(id: Int, deck: Queue[Int]) {
    def haveCard: Boolean = deck.nonEmpty
    def nextCard(): (Int, Player) = {
      val (card, d) = deck.dequeue
      (card, Player(id, d))
    }
    def deckSize: Int = deck.size
    def roundWon(winner: Int, looser: Int): Player = Player(id, deck.enqueueAll(List(winner, looser)))
    def lostByInfiniteLoop(): Player = Player(id, Queue.empty)
    def startRecursiveGame(card: Int): Player = Player(id, deck.take(card))
    def score: Int = deck.reverse.zip(Range(1, deck.length + 1)).map(points => points._1 * points._2).sum
  }

  case class Game(player1: Player, player2: Player) {
    override def toString: String = {
      s"Game($newLine 1:$player1$newLine 2:$player2$newLine)"
    }

    def winningPlayer: Player = if (player1.deck.isEmpty) player2 else player1
  }

  object Game {
    def apply(input: List[String]): Game = {
      val (p1Str, p2Str) = input.span(!_.isBlank)
      Game(Player.parse(1, p1Str), Player.parse(2, p2Str))
    }

    def play(game: Game): Game = play(game.player1, game.player2)

    @tailrec
    def play(player1: Player, player2: Player): Game = {
      if (player1.haveCard && player2.haveCard) { (player1.nextCard(), player2.nextCard()) match {
        case ((p1Card, p1), (p2Card, p2)) if p1Card > p2Card => play(p1.roundWon(p1Card, p2Card), p2)
        case ((p1Card, p1), (p2Card, p2)) => play(p1, p2.roundWon(p2Card, p1Card))
      }} else Game(player1, player2)
    }

    def recursivePlay(g: Game, prevGames: Set[Game] = Set.empty): Game = {
      if (prevGames.contains(g)) {
        Game(g.player1, g.player2.lostByInfiniteLoop())
      }
      else if (g.player1.haveCard && g.player2.haveCard) {
        val (p1Card, p1) = g.player1.nextCard()
        val (p2Card, p2) = g.player2.nextCard()
        if (p1Card <= p1.deckSize && p2Card <= p2.deckSize) {
          recursivePlay(Game(p1.startRecursiveGame(p1Card), p2.startRecursiveGame(p2Card))).winningPlayer.id match {
            case 1 => recursivePlay(Game(p1.roundWon(p1Card, p2Card), p2), prevGames + g)
            case 2 => recursivePlay(Game(p1, p2.roundWon(p2Card, p1Card)), prevGames + g)
          }
        } else if (p1Card > p2Card) recursivePlay(Game(p1.roundWon(p1Card, p2Card), p2), prevGames + g)
        else recursivePlay(Game(p1, p2.roundWon(p2Card, p1Card)), prevGames + g)
      } else Game(g.player1, g.player2)
    }

  }

  object Player {
    def parse(id: Int, deckStr: List[String]): Player = {
      Player(id, Queue(deckStr.map(card => Try(card.toInt)).filter(_.isSuccess).map(_.get): _*))
    }
  }

  solve()
}
