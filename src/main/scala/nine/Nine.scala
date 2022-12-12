package nine

import one.FileReader

import scala.annotation.tailrec

trait Direction
case object Right extends Direction
case object Left extends Direction
case object Up extends Direction
case object Down extends Direction

case class Coordinate(x: Int, y: Int)
case class Placeable(position: Coordinate) {
  def move(direction: Direction): Placeable = direction match {
    case Right => Placeable(position.copy(x = position.x + 1))
    case Left => Placeable(position.copy(x = position.x - 1))
    case Up => Placeable(position.copy(y = position.y + 1))
    case Down => Placeable(position.copy(y = position.y - 1))
  }

  def getDelta(other: Placeable): (Int, Int) = {
    (other.position.x - position.x, other.position.y - position.y)
  }

  def adjustTail(head: Placeable): Placeable = getDelta(head) match {
    case (dx, dy) if dx >= -1 && dx <= 1 && dy >= -1 && dy <= 1 => this
    case (dx, dy) if (dx == -2 || dx == 2) && (dy == -2 || dy == 2) =>
      Placeable(position.copy(x = head.position.x - (dx/2), y = head.position.y - (dy/2)))
    case (2, _) => Placeable(position.copy(x = head.position.x - 1, y = head.position.y))
    case (-2, _) => Placeable(position.copy(x = head.position.x + 1, y = head.position.y))
    case (_, 2) => Placeable(position.copy(x = head.position.x, y = head.position.y - 1))
    case (_, -2) => Placeable(position.copy(x = head.position.x, y = head.position.y + 1))
  }
}

case class Move(direction: Direction, steps: Int)

object Move {
  def apply(in: String): Move = {
    val split = in.split(" ")
    val steps = split.last.toInt
    split.head match {
      case "L" => Move(Left, steps)
      case "R" => Move(Right, steps)
      case "U" => Move(Up, steps)
      case "D" => Move(Down, steps)
    }
  }
}

object Nine {

  val largeRope: Seq[Placeable] = Seq.fill(10)(Placeable(Coordinate(0,0)))
  val startHead: Placeable = Placeable(Coordinate(0,0))
  val startTail: Placeable = Placeable(Coordinate(0,0))
  def parseToMoves(input: LazyList[String]): LazyList[Move] = {
    input.map(Move.apply)
  }

  def executeMove(move: Move, head: Placeable, tail: Placeable): Seq[(Placeable, Placeable)] = {
    @tailrec
    def loop(acc: Seq[(Placeable, Placeable)], head: Placeable, tail: Placeable, counter: Int): Seq[(Placeable, Placeable)] = {
      if (counter > move.steps) acc
      else {
        val movedHead = head.move(move.direction)
        val movedTail = tail.adjustTail(movedHead)
        loop(acc.appended((movedHead, movedTail)), movedHead, movedTail, counter + 1)
      }
    }

    loop(Seq.empty[(Placeable, Placeable)], head, tail, 1)
  }

  def executeAllMoves(input: LazyList[Move]): Seq[(Placeable, Placeable)] = {
    input.foldLeft(Seq((startHead, startTail))) { case (acc, move) =>
      val (head , tail) = acc.last
      acc ++ executeMove(move, head, tail)
    }
  }

  def countUniquePositionsTail(in: Seq[(Placeable, Placeable)]): Int = {
    in.map(_._2).distinct.length
  }

  def doPart1(): Int = {
    countUniquePositionsTail(executeAllMoves(parseToMoves(FileReader.lines.to(LazyList))))
  }

  def executeMoveOnLargeRope(move: Move, rope: Seq[Placeable]): Seq[Seq[Placeable]] = {
    @tailrec
    def loop(acc: Seq[Seq[Placeable]], currentRope: Seq[Placeable], left: Seq[Placeable], counter: Int): Seq[Seq[Placeable]] = {
      if(counter > move.steps) acc
      else { left match {
        case h :: Nil =>
          val updatedTailRope = currentRope.appended(h.adjustTail(currentRope.last))
          val movedHead = currentRope.head.move(move.direction)
          val newLeft = updatedTailRope.tail
          loop(acc.appended(updatedTailRope), Seq(movedHead), newLeft, counter + 1)
        case h :: t =>
          loop(acc, currentRope.appended(h.adjustTail(currentRope.last)), t, counter)
      }}
    }

    loop(Seq.empty[Seq[Placeable]], Seq(rope.head.move(move.direction)), rope.tail, 1)
  }

  def executeAllMovesLargeRope(input: LazyList[Move]): Seq[Seq[Placeable]] = {
    input.foldLeft(Seq(largeRope)) { case (acc, move) =>
      acc ++ executeMoveOnLargeRope(move, acc.last)
    }
  }
  
  def countTailPositionsLargeRope(input: Seq[Seq[Placeable]]): Int = {
    input.map(_.last).distinct.length
  }
  
  def doPart2(): Int = {
    countTailPositionsLargeRope(executeAllMovesLargeRope(parseToMoves(FileReader.lines.to(LazyList))))
  }

}
