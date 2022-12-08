package five

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FiveSpec extends AnyFlatSpec with Matchers {
  behavior of "Five"

  it should "extract the info" in {
    Five.extractMoves("move 13 from 2 to 1") shouldBe (13,2,1)
  }

  it should "perform a move" in {
    val newMap = Five.performTheMoves(LazyList((3,2,1)), Five.startStack)
    newMap(2) shouldBe Seq()
    newMap(1) shouldBe Seq("M", "J", "C", "B", "F", "R", "L", "H", "D", "C", "Z")
  }

//  it should "do part 1" in {
//    Five.doPart1() shouldBe "TQRFCBSJJ"
//  }

  it should "do part 2" in {
    Five.doPart2() shouldBe "RMHFJNVFP"
  }
}
