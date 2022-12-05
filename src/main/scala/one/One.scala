package one

import scala.io.Source

object  FileReader:
  private val filename = "input.txt"
  private val resource = Source.fromResource(filename)

  val lines: Iterator[String] = resource.getLines


object One:

  def accumulateCalories(input: Iterator[String]): Seq[Int] = {
    val (acc, som) = input.foldLeft((Seq.empty[Int], 0)) { case ((acc, sum), cal) =>
      if(cal.isEmpty) {
        (acc ++ Seq(sum), 0)
      } else {
        (acc, sum + cal.toInt)
      }             
    }

    acc ++ Seq(som)
  }

  def sort(input: Seq[Int]): Seq[Int] = {
    input.sorted.reverse
  }

  def part1: Int = {
    accumulateCalories(FileReader.lines).max
  }

  def part2 = {
    sort(accumulateCalories(FileReader.lines)).take(3).sum
  }


