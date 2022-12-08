package four

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FourSpec extends AnyFlatSpec with Matchers {
  behavior of "Four"

  it should "transform string in ranges" in {
    val in = LazyList("2-9,9-51","33-51,45-73")
    Four.transformToRanges(in) shouldBe LazyList((Range(2,9),Range(9,51)), (Range(33,51),Range(45,73)))
  }

  it should "determine if ranges are completely overlapping" in {
    val r1 = Range(2,9)
    val r2 = Range(9,51)
    val r3 = Range(9,49)

    r1.isInsideOtherRange(r2) shouldBe false
    r3.isInsideOtherRange(r2) shouldBe true
    r2.isInsideOtherRange(r3) shouldBe false

    r1.getList shouldBe Seq(2,3,4,5,6,7,8,9)
  }

  it should "filter overlapping ranges" in {
    val in = LazyList((Range(2,9),Range(3,9)), (Range(33,51),Range(45,73)))
    Four.countOverlappingRanges(in) shouldBe 1
  }

//  it should "do part 1" in {
//    Four.doPart1() shouldBe 450
//  }

  it should "get the overlap sections" in {
    val in = LazyList((Range(2,9),Range(3,9)), (Range(33,51),Range(45,73)))
    Four.getOverlapSections(in) shouldBe LazyList(Vector(3, 4, 5, 6, 7, 8, 9), Vector(45, 46, 47, 48, 49, 50, 51))
  }

  it should "count distinct sections" in {
    val in = LazyList(Vector(3, 4, 5, 6, 7, 8, 9), Vector(3, 46, 47, 48, 49, 50, 51))
    Four.countDistinctSections(in) shouldBe 2
  }

  it should "do part 2" in {
    Four.doPart2() shouldBe 837
  }
}
