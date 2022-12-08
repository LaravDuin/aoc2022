package five

import one.FileReader

import scala.util.matching.Regex

object Five:
  val startStack: Map[Int, Seq[String]] = Map(1 -> Seq("M", "J", "C", "B", "F", "R", "L", "H"), 2 -> Seq("Z", "C", "D"), 3 -> Seq("H", "J", "F", "C", "N", "G", "W"), 4 -> Seq("P", "J", "D", "M", "T", "S", "B"), 5 -> Seq("N", "C", "D", "R", "J"), 6 -> Seq("W", "L", "D", "Q", "P", "J", "G", "Z"), 7-> Seq("P", "Z", "T", "F", "R", "H"), 8-> Seq("L", "V", "M", "G"), 9-> Seq("C", "B", "G", "P", "F", "Q", "R", "J"))
  val regex: Regex = raw"move (\d*) from (\d) to (\d)".r

  def extractMoves(in: String): (Int, Int, Int) = in match {
    case regex(move, from, to) => (move.toInt, from.toInt, to.toInt)
    case _ => (0,0,0)
  }

  def performTheMoves(input: LazyList[(Int, Int, Int)], startMap: Map[Int, Seq[String]]): Map[Int, Seq[String]] = {
    input.foldLeft(startMap) { case (acc, (amount, from, to)) =>
      val left: Seq[String] = acc(from).take(acc(from).length-amount)
      val toMove: Seq[String] = acc(from).takeRight(amount).reverse
      val newList: Seq[String] = acc(to) ++ toMove
      val newAcc: Map[Int, Seq[String]] = acc ++ Map(from -> left) ++ Map(to -> newList)
      newAcc
    }
  }

  def performTheMovesMultiple(input: LazyList[(Int, Int, Int)], startMap: Map[Int, Seq[String]]): Map[Int, Seq[String]] = {
    input.foldLeft(startMap) { case (acc, (amount, from, to)) =>
      val left: Seq[String] = acc(from).take(acc(from).length-amount)
      val toMove: Seq[String] = acc(from).takeRight(amount)
      val newList: Seq[String] = acc(to) ++ toMove
      val newAcc: Map[Int, Seq[String]] = acc ++ Map(from -> left) ++ Map(to -> newList)
      newAcc
    }
  }

  def doPart1(): String = {
    val newMap = performTheMoves(FileReader.lines.to(LazyList).map(extractMoves), startStack)
    (1 to 9).map { i =>
      newMap(i).last
    }.mkString
  }

  def doPart2(): String = {
    val newMap = performTheMovesMultiple(FileReader.lines.to(LazyList).map(extractMoves), startStack)
    (1 to 9).map { i =>
      newMap(i).last
    }.mkString
  }