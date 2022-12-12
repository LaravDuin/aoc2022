package ten

import one.FileReader

case class Cycle(value: Long, nr: Int) {
  def increaseNoop(): Cycle = Cycle(value, nr + 1)
  def increaseAddx(toAdd: Int): Seq[Cycle] = {
    Seq(Cycle(value, nr + 1), Cycle(value + toAdd, nr + 2))
  }
  def isVisible(position: Int): Boolean = {
    Math.abs(value - position) < 2
  }
}

object Ten {

  val startCycle: Cycle = Cycle(1L, 1)
  val targetCycles: Seq[Int] = Seq(20, 60, 100, 140, 180, 220)

  def executeCycle(currentCycle: Cycle, input: String): Seq[Cycle] = input match {
    case "noop" => Seq(currentCycle.increaseNoop())
    case addx => currentCycle.increaseAddx(addx.split(" ").last.toInt)
  }

  def fetchTargetCycles(input: LazyList[String]): Seq[Cycle] = {
    input.foldLeft((Seq.empty[Cycle], startCycle)) { case ((acc, currentCycle), input) =>
      val newCycles = executeCycle(currentCycle, input)
      (acc ++ newCycles.filter(cycle => targetCycles.contains(cycle.nr)), newCycles.last)
    }._1
  }

  def writeNextPixel(cycle: Cycle, currentInput: String): String = {
    val positionToWrite = currentInput.length
    if(cycle.isVisible(positionToWrite)) {
      currentInput + "#"
    } else {
      currentInput + "."
    }
  }

  def generatePixelOutPut(input: LazyList[String]): Seq[String] = {
    input.foldLeft((Seq(startCycle), startCycle)) { case ((acc, currentCycle), input) =>
      val newCycles = executeCycle(currentCycle, input)
      (acc ++ newCycles, newCycles.last)
    }._1.foldLeft((Seq.empty[String], "")) { case ((acc, current), cycle) =>
      if((cycle.nr - 1) % 40 == 0 && cycle.nr != 1) {
        (acc.appended(current), writeNextPixel(cycle, ""))
      } else {
        (acc, writeNextPixel(cycle, current))
      }
    }._1
  }

  def doPart1(): Long = {
    fetchTargetCycles(FileReader.lines.to(LazyList)).map(c => c.nr * c.value).sum
  }
  
  def doPart2(): Unit = {
    generatePixelOutPut(FileReader.lines.to(LazyList))
      .foreach(println)
  }

}
