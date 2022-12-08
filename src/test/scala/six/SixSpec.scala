package six

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SixSpec extends AnyFlatSpec with Matchers {

  behavior of "Six"

  it should "create an indexed sliding window" in {
    val in = "asdkeoiaoseinf"
    Six.slidingWindowWithIndex(in, 4) shouldBe LazyList(("asdk",0), ("sdke",1), ("dkeo",2), ("keoi",3), ("eoia",4), ("oiao",5), ("iaos",6), ("aose",7), ("osei",8), ("sein",9), ("einf",10))
  }

//  it should "do part 1" in {
//    Six.doPart1() shouldBe 1920
//  }

    it should "do part 2" in {
      Six.doPart2() shouldBe 2334
    }

}
