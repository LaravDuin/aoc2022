package eigth

import one.FileReader

import scala.annotation.tailrec

case class Tree(height: Int, left: Option[Seq[Int]], right: Option[Seq[Int]], top: Option[Seq[Int]], bottom: Option[Seq[Int]]) {
  def isVisible: Boolean = {
    left.forall(_.forall(_ < height)) || right.forall(_.forall(_ < height)) || top.forall(_.forall(_ < height)) || bottom.forall(_.forall(_ < height))
  }

  def getScenicScore: Int = {
    @tailrec
    def loop(acc: Int, left: Seq[Int]): Int = left match {
      case Nil => acc
      case list if list.head >= height => acc + 1
      case list => loop(acc + 1, list.tail)
    }
    val leftScore: Int = left.map(l => loop(0, l.reverse)).getOrElse(0)
    val rightScore: Int = right.map(r => loop(0, r)).getOrElse(0)
    val topScore: Int = top.map(t => loop(0, t.reverse)).getOrElse(0)
    val bottomScore: Int = bottom.map(b => loop(0, b)).getOrElse(0)
    leftScore * rightScore * topScore * bottomScore
  }
}
case class Coordinate(x: Int, y: Int) {
  def getOneAbove: Coordinate = {
    Coordinate(x, y - 1)
  }

  def getOneBelow: Coordinate = {
    Coordinate(x, y + 1)
  }
}

object Eight {

  val treeMap: Map[Coordinate, Tree] = Map.empty[Coordinate, Tree]
  val maxX: LazyList[String] => Int = list => list.head.length - 1
  val maxY: LazyList[String] => Int = list => list.length - 1

  def buildTreeMap(input: LazyList[String]): Map[Coordinate, Tree] = {
    val emptyMap: Map[Coordinate, Tree] = Map.empty[Coordinate, Tree]
    val coordinatedList: Seq[(IndexedSeq[(Char, Int)], Int)] = input.zipWithIndex.map { case (line, y) =>
      (line.zipWithIndex, y)
    }

    @tailrec
    def loop(acc: Map[Coordinate, Tree], left: Seq[(Char, Int)], right: Seq[(Char, Int)], y: Int): Map[Coordinate, Tree] = right match {
      case Nil => acc
      case list =>
        val newAcc = acc ++
        Map(Coordinate(list.head._2, y) ->
          Tree(list.head._1.asDigit,
            Option(left.map(_._1.asDigit)).filter(_.nonEmpty),
            Option(list.tail.map(_._1.asDigit)).filter(_.nonEmpty),
            None,
            None
          ))
        loop(newAcc, left.appended(list.head), list.tail, y)
    }

    def buildTopAndBottom(map: Map[Coordinate, Tree]): Map[Coordinate, Tree] = {
      map.foldLeft(map) { case (acc, (coor, tree)) =>
        val topCoors = (0 to coor.y).filterNot(_ == coor.y).map( y => Coordinate(coor.x, y))
        val bottomCoors = (coor.y to maxY(input)).filterNot(_ == coor.y).map( y => Coordinate(coor.x, y))
        val topList = Option(topCoors).filter(_.nonEmpty).map(_.map(xy => map(xy)).map(_.height))
        val bottomList = Option(bottomCoors).filter(_.nonEmpty).map(_.map(xy => map(xy)).map(_.height))
        val newTree = tree.copy(top = topList, bottom = bottomList)
        acc ++ Map(coor -> newTree)
      }
    }

    val buildLeftRight = coordinatedList.foldLeft[Map[Coordinate, Tree]](emptyMap) { case (acc, (list, y)) =>
      loop(acc, Seq.empty[(Char, Int)], list, y)
    }

    buildTopAndBottom(buildLeftRight)
  }

  def countVisibleTrees(treeMap: Map[Coordinate, Tree]): Int = {
    treeMap.values.count(_.isVisible)
  }

  def doPart1(): Int = {
    val treeMap = buildTreeMap(FileReader.lines.to(LazyList))
    countVisibleTrees(treeMap)
  }
  
  def getBestScore(treeMap: Map[Coordinate, Tree]): Int = {
    treeMap.values.map(_.getScenicScore).max
  }
  
  def doPart2(): Int = {
    val treeMap = buildTreeMap(FileReader.lines.to(LazyList))
    getBestScore(treeMap)
  }
}
