package three

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ThreeSpec extends AnyFlatSpec with Matchers {

  behavior of "Three"

  it should "retrieve the intersection of start and end string" in {
    val in = LazyList("GGVGlqWFgVfFqqVZGFlblJPMsDbbMrDMpDsJRn")
    Three.intersectInput(in) shouldBe LazyList('l')
  }

  it should "convert to value" in {
    val in = LazyList('a', 'A', 'z', 'Z')
    Three.calculateValue(in) shouldBe LazyList(1,27,26,52)
  }

//  it should "do part 1" in {
//    Three.part1() shouldBe 8233
//  }

  it should "group by 3" in {
    val in = LazyList("GGVGlqWFgVfFqqVZGFlblJPMsDbbMrDMpDsJRn","LwzHtwdLHHwDrzPZzzsJbJ","wdLTBvSvHvZVGCjhfN","HsSSnZVHjjssZnJpSJjBHHWgQGcgqqQLQdQFqNgWgqGNDg","rmmRwrtfThtTrbCrGGGcLBDTqDBNQLdL","mwPrrbzPfwvbzhwMMnnjHnBjZlnzMM")
    Three.getSlidingWindow(in) shouldBe LazyList(LazyList("GGVGlqWFgVfFqqVZGFlblJPMsDbbMrDMpDsJRn", "LwzHtwdLHHwDrzPZzzsJbJ", "wdLTBvSvHvZVGCjhfN"), LazyList("HsSSnZVHjjssZnJpSJjBHHWgQGcgqqQLQdQFqNgWgqGNDg", "rmmRwrtfThtTrbCrGGGcLBDTqDBNQLdL", "mwPrrbzPfwvbzhwMMnnjHnBjZlnzMM"))
  }

  it should "get the common item" in {
    val in = LazyList(LazyList("GGVGlqWFgVfFqqVZGFlblJPMsDbbMrDMpDsJRn", "LwzHtwdLHHwDrzPZzzsJbJ", "wdLTBvSvHvZVGCjhfN"), LazyList("HsSSnZVHjjssZnJpSJjBHHWgQGcgqqQLQdQFqNgWgqGNDg", "rmmRwrtfThtTrbCrGGGcLBDTqDBNQLdL", "mwPrrbzPfwvbzhwMMnnjHnBjZlnzMM"))
    Three.intersectGroups(in) shouldBe LazyList('Z', 'B')
  }

  it should "do part 2" in {
    Three.part2() shouldBe 2821
  }
}
