package two

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TwoSpec extends AnyFlatSpec with Matchers {

  behavior of "Two"

  it should "parse the moves" in {
    val input = LazyList("A X", "B Y", "C Z", "A Z")
    Two.parseMoves(input) shouldBe LazyList(Game(Rock,Scissors), Game(Paper,Paper), Game(Scissors,Rock), Game(Rock,Paper))
  }

  it should "fetch scores" in {
    val in = LazyList(Game(Rock,Rock), Game(Paper,Paper), Game(Scissors,Scissors), Game(Rock,Scissors))
    Two.calculateScores(in) shouldBe LazyList(4, 5, 6, 3)
  }

//  it should "do part 1" in {
//    Two.part1() shouldBe 8890
//  }

  it should "do part 2" in {
    Two.part2() shouldBe 10238
  }

}
