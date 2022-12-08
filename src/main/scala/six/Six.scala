package six

import one.FileReader

object Six:

  def slidingWindowWithIndex(input: String, windowSize: Int): LazyList[(String, Int)] = {
    input.sliding(windowSize).zipWithIndex.to(LazyList)
  }

  def findUniqueString(in: (String, Int), windowSize: Int): Boolean = {
    in._1.toCharArray.distinct.length == windowSize
  }
  
  def getIndex(input: String, windowSize: Int): Int = {
    val (_, index) = slidingWindowWithIndex(input, windowSize).dropWhile(i => !findUniqueString(i, windowSize)).head
    index + windowSize
  }

  def doPart1(): Int = {
    getIndex(FileReader.lines.next(), 4)
  }

  def doPart2(): Int = {
    getIndex(FileReader.lines.next(), 14)
  }
