package three

import one.FileReader

object Three {

  def intersectInput(input: LazyList[String]): LazyList[Char] = {
    input.foldLeft(LazyList.empty[Char]) { case (acc, str) =>
      val start = str.substring(0, str.length/2)
      val end = str.substring(str.length/2)
      acc ++ LazyList(start.intersect(end).head)
    }
  }

  def calculateValue(input: LazyList[Char]): LazyList[Int] = {
    input.map { ch =>
      if(ch.isUpper) {
        (ch & 31) + 26
      } else {
        ch & 31
      }
    }
  }

  def part1(): Int = {
    calculateValue(intersectInput(FileReader.lines.to(LazyList))).sum
  }

  def getSlidingWindow(input: LazyList[String]): LazyList[Seq[String]] = {
    input.sliding(3,3).to(LazyList)
  }

  def intersectGroups(input: LazyList[Seq[String]]): LazyList[Char] = {
    input.map { group => group.tail.foldLeft(group.head) { case (acc, str) =>
        acc.intersect(str)
      }.head
    }
  }
  
  def part2(): Int = {
    calculateValue(intersectGroups(getSlidingWindow(FileReader.lines.to(LazyList)))).sum
  }

}
