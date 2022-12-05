package two

import one.FileReader
import two.Game.scoreMap

trait Move {
  val win: Move
  val draw: Move
  val loose: Move
}

case object Paper extends Move {
  val win = Scissors
  val draw = Paper
  val loose = Rock

}

case object Scissors extends Move {
  val win = Rock
  val draw = Scissors
  val loose = Paper

}

case object Rock extends Move {
  val win = Paper
  val draw = Rock
  val loose = Scissors

}

case class Game(opponent: Move, me: Move) {
  val isWon: Boolean = Game.winningCombinations.contains((opponent, me))
  val isDraw: Boolean = opponent == me
  val score: Int = if(isWon) scoreMap(me) + 6
    else if(isDraw) scoreMap(me) + 3
    else scoreMap(me)
}

object Game:
  val scoreMap: Map[Move, Int] = Map(Paper -> 2, Rock -> 1, Scissors -> 3)
  val winningCombinations: Seq[(Move, Move)] = Seq((Paper, Scissors), (Scissors, Rock), (Rock, Paper))

object Parsing:
  val toMove: Map[String, Move] = Map("A" -> Rock, "B" -> Paper, "C" -> Scissors)
  val getMyMove: (Move, String) => Move = (opponent, x) => x match {
    case "X" => opponent.loose
    case "Y" => opponent.draw
    case "Z" => opponent.win
  }

object Two {

  def parseMoves(input: LazyList[String]): LazyList[Game] = {
    input.map(_.split(" ")).map { i =>
      val opponent = Parsing.toMove(i.head)
      val me = Parsing.getMyMove(opponent, i.last)
      Game(opponent, me)
    }
  }

  def calculateScores(input: LazyList[Game]): LazyList[Int] = {
    input.map(_.score)
  }

  def part1(): Int = {
    calculateScores(parseMoves(FileReader.lines.to(LazyList))).sum
  }
  
  def part2(): Int = {
    calculateScores(parseMoves(FileReader.lines.to(LazyList))).sum
  }
}
