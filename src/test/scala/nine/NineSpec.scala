package nine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NineSpec extends AnyFlatSpec with Matchers {
  behavior of "Nine"

  it should "get all positions of head and tail after a move" in {
    val in = "R 4"
    val move = Move(in)
    Nine.executeMove(move, Nine.startHead, Nine.startTail) shouldBe List((Placeable(Coordinate(1,0)),Placeable(Coordinate(0,0))), (Placeable(Coordinate(2,0)),Placeable(Coordinate(1,0))), (Placeable(Coordinate(3,0)),Placeable(Coordinate(2,0))), (Placeable(Coordinate(4,0)),Placeable(Coordinate(3,0))))
  }

  it should "make all moves" in {
    val in = LazyList(
    "R 4",
    "U 4",
    "L 3",
    "D 1",
    "R 4",
    "D 1",
    "L 5",
    "R 2"
    )
    val moves = Nine.parseToMoves(in)
    Nine.executeAllMoves(moves) shouldBe List((Placeable(Coordinate(0,0)),Placeable(Coordinate(0,0))),
      (Placeable(Coordinate(1,0)),Placeable(Coordinate(0,0))), (Placeable(Coordinate(2,0)),Placeable(Coordinate(1,0))), (Placeable(Coordinate(3,0)),Placeable(Coordinate(2,0))), (Placeable(Coordinate(4,0)),Placeable(Coordinate(3,0))),
      (Placeable(Coordinate(4,1)),Placeable(Coordinate(3,0))), (Placeable(Coordinate(4,2)),Placeable(Coordinate(4,1))), (Placeable(Coordinate(4,3)),Placeable(Coordinate(4,2))), (Placeable(Coordinate(4,4)),Placeable(Coordinate(4,3))),
      (Placeable(Coordinate(3,4)),Placeable(Coordinate(4,3))), (Placeable(Coordinate(2,4)),Placeable(Coordinate(3,4))), (Placeable(Coordinate(1,4)),Placeable(Coordinate(2,4))),
      (Placeable(Coordinate(1,3)),Placeable(Coordinate(2,4))),
      (Placeable(Coordinate(2,3)),Placeable(Coordinate(2,4))), (Placeable(Coordinate(3,3)),Placeable(Coordinate(2,4))), (Placeable(Coordinate(4,3)),Placeable(Coordinate(3,3))), (Placeable(Coordinate(5,3)),Placeable(Coordinate(4,3))), (Placeable(Coordinate(5,2)),Placeable(Coordinate(4,3))), (Placeable(Coordinate(4,2)),Placeable(Coordinate(4,3))), (Placeable(Coordinate(3,2)),Placeable(Coordinate(4,3))), (Placeable(Coordinate(2,2)),Placeable(Coordinate(3,2))), (Placeable(Coordinate(1,2)),Placeable(Coordinate(2,2))), (Placeable(Coordinate(0,2)),Placeable(Coordinate(1,2))), (Placeable(Coordinate(1,2)),Placeable(Coordinate(1,2))), (Placeable(Coordinate(2,2)),Placeable(Coordinate(1,2))))
  }

  it should "count all unique tail positions" in {
    val in = List((Placeable(Coordinate(0,0)),Placeable(Coordinate(0,0))), (Placeable(Coordinate(1,0)),Placeable(Coordinate(0,0))), (Placeable(Coordinate(2,0)),Placeable(Coordinate(1,0))), (Placeable(Coordinate(3,0)),Placeable(Coordinate(2,0))), (Placeable(Coordinate(4,0)),Placeable(Coordinate(3,0))), (Placeable(Coordinate(4,1)),Placeable(Coordinate(3,0))), (Placeable(Coordinate(4,2)),Placeable(Coordinate(4,1))), (Placeable(Coordinate(4,3)),Placeable(Coordinate(4,2))), (Placeable(Coordinate(4,4)),Placeable(Coordinate(4,3))), (Placeable(Coordinate(3,4)),Placeable(Coordinate(4,3))), (Placeable(Coordinate(2,4)),Placeable(Coordinate(3,4))), (Placeable(Coordinate(1,4)),Placeable(Coordinate(2,4))), (Placeable(Coordinate(1,3)),Placeable(Coordinate(2,4))), (Placeable(Coordinate(2,3)),Placeable(Coordinate(2,4))), (Placeable(Coordinate(3,3)),Placeable(Coordinate(2,4))), (Placeable(Coordinate(4,3)),Placeable(Coordinate(3,3))), (Placeable(Coordinate(5,3)),Placeable(Coordinate(4,3))), (Placeable(Coordinate(5,2)),Placeable(Coordinate(4,3))), (Placeable(Coordinate(4,2)),Placeable(Coordinate(4,3))), (Placeable(Coordinate(3,2)),Placeable(Coordinate(4,3))), (Placeable(Coordinate(2,2)),Placeable(Coordinate(3,2))), (Placeable(Coordinate(1,2)),Placeable(Coordinate(2,2))), (Placeable(Coordinate(0,2)),Placeable(Coordinate(1,2))), (Placeable(Coordinate(1,2)),Placeable(Coordinate(1,2))), (Placeable(Coordinate(2,2)),Placeable(Coordinate(1,2))))
    Nine.countUniquePositionsTail(in) shouldBe 13
  }

//  it should "do Part 1" in {
//    Nine.doPart1() shouldBe 6181
//  }

  it should "adjust a fully diagonal tail" in {
    val head = Placeable(Coordinate(2,3))
    val tail = Placeable(Coordinate(4,1))
    tail.adjustTail(head) shouldBe Placeable(Coordinate(3,2))
  }

  it should "execute move on large rope" in {
    val in = "R 4"
    val move = Move(in)
    Nine.executeMoveOnLargeRope(move, Nine.largeRope) shouldBe List(List(Placeable(Coordinate(1,0)), Placeable(Coordinate(0,0)), Placeable(Coordinate(0,0)), Placeable(Coordinate(0,0)), Placeable(Coordinate(0,0)), Placeable(Coordinate(0,0)), Placeable(Coordinate(0,0)), Placeable(Coordinate(0,0)), Placeable(Coordinate(0,0)), Placeable(Coordinate(0,0))), List(Placeable(Coordinate(2,0)), Placeable(Coordinate(1,0)), Placeable(Coordinate(0,0)), Placeable(Coordinate(0,0)), Placeable(Coordinate(0,0)), Placeable(Coordinate(0,0)), Placeable(Coordinate(0,0)), Placeable(Coordinate(0,0)), Placeable(Coordinate(0,0)), Placeable(Coordinate(0,0))), List(Placeable(Coordinate(3,0)), Placeable(Coordinate(2,0)), Placeable(Coordinate(1,0)), Placeable(Coordinate(0,0)), Placeable(Coordinate(0,0)), Placeable(Coordinate(0,0)), Placeable(Coordinate(0,0)), Placeable(Coordinate(0,0)), Placeable(Coordinate(0,0)), Placeable(Coordinate(0,0))), List(Placeable(Coordinate(4,0)), Placeable(Coordinate(3,0)), Placeable(Coordinate(2,0)), Placeable(Coordinate(1,0)), Placeable(Coordinate(0,0)), Placeable(Coordinate(0,0)), Placeable(Coordinate(0,0)), Placeable(Coordinate(0,0)), Placeable(Coordinate(0,0)), Placeable(Coordinate(0,0))))
  }

  it should "calculate the example correctly" in {
    val in = LazyList(
    "R 5",
    "U 8",
    "L 8",
    "D 3",
    "R 17",
    "D 10",
    "L 25",
    "U 20"
    )
    val moves = Nine.parseToMoves(in)
    val executedMoves = Nine.executeAllMovesLargeRope(moves)
    println(executedMoves)
    Nine.countTailPositionsLargeRope(executedMoves) shouldBe 36
  }

  it should "do part 2" in {
    Nine.doPart2() shouldBe 2386
  }
  
}
