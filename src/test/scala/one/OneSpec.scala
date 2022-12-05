package one

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OneSpec extends AnyFlatSpec with Matchers {

  behavior of "One"

  it should "sum calories" in {
    val input = Seq("1", "2", "", "3", "", "3", "4", "6").iterator
    val expected = Seq(3, 3, 13)

    One.accumulateCalories(input) shouldBe expected
  }

//  it should "solve part 1" in {
//    One.part1 shouldBe 67016
//  }

  it should "sort" in {
    val in = Seq(1,5,2,7,4)
    One.sort(in) shouldBe Seq(7,5,4,2,1)
  }

  it should "solve part 2" in {
    One.part2 shouldBe 200116
  }
}
