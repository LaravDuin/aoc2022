package four

import one.FileReader

case class Range(start: Int, stop: Int) {
  def isInsideOtherRange(other: Range): Boolean = {
    start >= other.start && stop <= other.stop
  }
  
  val getList: Seq[Int] = start to stop
}

object Range:
  def apply(raw: String): (Range, Range) = {
    val split = raw.split(",")
    val first = split.head.split("-").map(_.toInt)
    val second = split.last.split("-").map(_.toInt)
    (Range(first.head, first.last), Range(second.head, second.last))
  }

object Four:
  def transformToRanges(input: LazyList[String]): LazyList[(Range, Range)] = {
    input.map(Range.apply)
  }

  def countOverlappingRanges(input: LazyList[(Range, Range)]): Int = {
    input.count { case (r1 , r2) =>
      r1.isInsideOtherRange(r2) || r2.isInsideOtherRange(r1)
    }
  }

  def doPart1(): Int = {
    countOverlappingRanges(transformToRanges(FileReader.lines.to(LazyList)))
  }
  
  def getOverlapSections(input: LazyList[(Range, Range)]): LazyList[Seq[Int]] = {
    input.map { case (r1, r2) =>
      r1.getList.intersect(r2.getList)
    }
  }
  
  def countDistinctSections(input: LazyList[Seq[Int]]): Int = {
    input.filterNot(_.isEmpty).length
  }
  
  def doPart2(): Int = {
    countDistinctSections(getOverlapSections(transformToRanges(FileReader.lines.to(LazyList))))
  }


