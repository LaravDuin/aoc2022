package eigth

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EightSpec extends AnyFlatSpec with Matchers {

  behavior of "Eight"

  it should "build the treeMap" in {
    val input = LazyList(
      "30373",
      "25512",
      "65332",
      "33549",
      "35390"
    )

    Eight.buildTreeMap(input) shouldBe Map(Coordinate(1, 0) -> Tree(0, Some(List(3)), Some(Vector(3, 7, 3)), None, Some(Vector(5, 5, 3, 5))), Coordinate(4, 3) -> Tree(9, Some(List(3, 3, 5, 4)), None, Some(Vector(3, 2, 2)), Some(Vector(0))), Coordinate(3, 4) -> Tree(9, Some(List(3, 5, 3)), Some(Vector(0)), Some(Vector(7, 1, 3, 4)), None), Coordinate(0, 4) -> Tree(3, None, Some(Vector(5, 3, 9, 0)), Some(Vector(3, 2, 6, 3)), None), Coordinate(4, 0) -> Tree(3, Some(List(3, 0, 3, 7)), None, None, Some(Vector(2, 2, 9, 0))), Coordinate(2, 1) -> Tree(5, Some(List(2, 5)), Some(Vector(1, 2)), Some(Vector(3)), Some(Vector(3, 5, 3))), Coordinate(1, 1) -> Tree(5, Some(List(2)), Some(Vector(5, 1, 2)), Some(Vector(0)), Some(Vector(5, 3, 5))), Coordinate(1, 4) -> Tree(5, Some(List(3)), Some(Vector(3, 9, 0)), Some(Vector(0, 5, 5, 3)), None), Coordinate(4, 1) -> Tree(2, Some(List(2, 5, 5, 1)), None, Some(Vector(3)), Some(Vector(2, 9, 0))), Coordinate(1, 3) -> Tree(3, Some(List(3)), Some(Vector(5, 4, 9)), Some(Vector(0, 5, 5)), Some(Vector(5))), Coordinate(2, 0) -> Tree(3, Some(List(3, 0)), Some(Vector(7, 3)), None, Some(Vector(5, 3, 5, 3))), Coordinate(4, 2) -> Tree(2, Some(List(6, 5, 3, 3)), None, Some(Vector(3, 2)), Some(Vector(9, 0))), Coordinate(3, 1) -> Tree(1, Some(List(2, 5, 5)), Some(Vector(2)), Some(Vector(7)), Some(Vector(3, 4, 9))), Coordinate(3, 3) -> Tree(4, Some(List(3, 3, 5)), Some(Vector(9)), Some(Vector(7, 1, 3)), Some(Vector(9))), Coordinate(0, 2) -> Tree(6, None, Some(Vector(5, 3, 3, 2)), Some(Vector(3, 2)), Some(Vector(3, 3))), Coordinate(3, 0) -> Tree(7, Some(List(3, 0, 3)), Some(Vector(3)), None, Some(Vector(1, 3, 4, 9))), Coordinate(0, 1) -> Tree(2, None, Some(Vector(5, 5, 1, 2)), Some(Vector(3)), Some(Vector(6, 3, 3))), Coordinate(2, 4) -> Tree(3, Some(List(3, 5)), Some(Vector(9, 0)), Some(Vector(3, 5, 3, 5)), None), Coordinate(4, 4) -> Tree(0, Some(List(3, 5, 3, 9)), None, Some(Vector(3, 2, 2, 9)), None), Coordinate(2, 2) -> Tree(3, Some(List(6, 5)), Some(Vector(3, 2)), Some(Vector(3, 5)), Some(Vector(5, 3))), Coordinate(0, 0) -> Tree(3, None, Some(Vector(0, 3, 7, 3)), None, Some(Vector(2, 6, 3, 3))), Coordinate(1, 2) -> Tree(5, Some(List(6)), Some(Vector(3, 3, 2)), Some(Vector(0, 5)), Some(Vector(3, 5))), Coordinate(2, 3) -> Tree(5, Some(List(3, 3)), Some(Vector(4, 9)), Some(Vector(3, 5, 3)), Some(Vector(3))), Coordinate(0, 3) -> Tree(3, None, Some(Vector(3, 5, 4, 9)), Some(Vector(3, 2, 6)), Some(Vector(3))), Coordinate(3, 2) -> Tree(3, Some(List(6, 5, 3)), Some(Vector(2)), Some(Vector(7, 1)), Some(Vector(4, 9))))
  }

  it should "determine if tree is visible" in {
    val tree1 = Tree(3, Some(List(6, 5)), Some(Vector(3, 2)), Some(Vector(3, 5)), Some(Vector(5, 3)))
    val tree2 = Tree(5, Some(List(3, 3)), Some(Vector(4, 9)), Some(Vector(3, 5, 3)), Some(Vector(3)))
    tree1.isVisible shouldBe false
    tree2.isVisible shouldBe true
  }

  it should "count visible trees" in {
    val input = Map(Coordinate(1, 0) -> Tree(0, Some(List(3)), Some(Vector(3, 7, 3)), None, Some(Vector(5, 5, 3, 5))), Coordinate(4, 3) -> Tree(9, Some(List(3, 3, 5, 4)), None, Some(Vector(3, 2, 2)), Some(Vector(0))), Coordinate(3, 4) -> Tree(9, Some(List(3, 5, 3)), Some(Vector(0)), Some(Vector(7, 1, 3, 4)), None), Coordinate(0, 4) -> Tree(3, None, Some(Vector(5, 3, 9, 0)), Some(Vector(3, 2, 6, 3)), None), Coordinate(4, 0) -> Tree(3, Some(List(3, 0, 3, 7)), None, None, Some(Vector(2, 2, 9, 0))), Coordinate(2, 1) -> Tree(5, Some(List(2, 5)), Some(Vector(1, 2)), Some(Vector(3)), Some(Vector(3, 5, 3))), Coordinate(1, 1) -> Tree(5, Some(List(2)), Some(Vector(5, 1, 2)), Some(Vector(0)), Some(Vector(5, 3, 5))), Coordinate(1, 4) -> Tree(5, Some(List(3)), Some(Vector(3, 9, 0)), Some(Vector(0, 5, 5, 3)), None), Coordinate(4, 1) -> Tree(2, Some(List(2, 5, 5, 1)), None, Some(Vector(3)), Some(Vector(2, 9, 0))), Coordinate(1, 3) -> Tree(3, Some(List(3)), Some(Vector(5, 4, 9)), Some(Vector(0, 5, 5)), Some(Vector(5))), Coordinate(2, 0) -> Tree(3, Some(List(3, 0)), Some(Vector(7, 3)), None, Some(Vector(5, 3, 5, 3))), Coordinate(4, 2) -> Tree(2, Some(List(6, 5, 3, 3)), None, Some(Vector(3, 2)), Some(Vector(9, 0))), Coordinate(3, 1) -> Tree(1, Some(List(2, 5, 5)), Some(Vector(2)), Some(Vector(7)), Some(Vector(3, 4, 9))), Coordinate(3, 3) -> Tree(4, Some(List(3, 3, 5)), Some(Vector(9)), Some(Vector(7, 1, 3)), Some(Vector(9))), Coordinate(0, 2) -> Tree(6, None, Some(Vector(5, 3, 3, 2)), Some(Vector(3, 2)), Some(Vector(3, 3))), Coordinate(3, 0) -> Tree(7, Some(List(3, 0, 3)), Some(Vector(3)), None, Some(Vector(1, 3, 4, 9))), Coordinate(0, 1) -> Tree(2, None, Some(Vector(5, 5, 1, 2)), Some(Vector(3)), Some(Vector(6, 3, 3))), Coordinate(2, 4) -> Tree(3, Some(List(3, 5)), Some(Vector(9, 0)), Some(Vector(3, 5, 3, 5)), None), Coordinate(4, 4) -> Tree(0, Some(List(3, 5, 3, 9)), None, Some(Vector(3, 2, 2, 9)), None), Coordinate(2, 2) -> Tree(3, Some(List(6, 5)), Some(Vector(3, 2)), Some(Vector(3, 5)), Some(Vector(5, 3))), Coordinate(0, 0) -> Tree(3, None, Some(Vector(0, 3, 7, 3)), None, Some(Vector(2, 6, 3, 3))), Coordinate(1, 2) -> Tree(5, Some(List(6)), Some(Vector(3, 3, 2)), Some(Vector(0, 5)), Some(Vector(3, 5))), Coordinate(2, 3) -> Tree(5, Some(List(3, 3)), Some(Vector(4, 9)), Some(Vector(3, 5, 3)), Some(Vector(3))), Coordinate(0, 3) -> Tree(3, None, Some(Vector(3, 5, 4, 9)), Some(Vector(3, 2, 6)), Some(Vector(3))), Coordinate(3, 2) -> Tree(3, Some(List(6, 5, 3)), Some(Vector(2)), Some(Vector(7, 1)), Some(Vector(4, 9))))
    Eight.countVisibleTrees(input) shouldBe 21
  }

  //  it should "do Part 1" in {
  //    Eight.doPart1() shouldBe 1854
  //  }

  it should "calculate the scenic score" in {
    val tree1 = Tree(0, Some(List(3)), Some(Vector(3, 7, 3)), None, Some(Vector(5, 5, 3, 5)))
    val tree2 = Tree(5, Some(List(2, 5)), Some(Vector(1, 2)), Some(Vector(3)), Some(Vector(3, 5, 3)))

    tree1.getScenicScore shouldBe 0
    tree2.getScenicScore shouldBe 4
  }

  it should "get the best score" in {
    val input = Map(Coordinate(1, 0) -> Tree(0, Some(List(3)), Some(Vector(3, 7, 3)), None, Some(Vector(5, 5, 3, 5))), Coordinate(4, 3) -> Tree(9, Some(List(3, 3, 5, 4)), None, Some(Vector(3, 2, 2)), Some(Vector(0))), Coordinate(3, 4) -> Tree(9, Some(List(3, 5, 3)), Some(Vector(0)), Some(Vector(7, 1, 3, 4)), None), Coordinate(0, 4) -> Tree(3, None, Some(Vector(5, 3, 9, 0)), Some(Vector(3, 2, 6, 3)), None), Coordinate(4, 0) -> Tree(3, Some(List(3, 0, 3, 7)), None, None, Some(Vector(2, 2, 9, 0))), Coordinate(2, 1) -> Tree(5, Some(List(2, 5)), Some(Vector(1, 2)), Some(Vector(3)), Some(Vector(3, 5, 3))), Coordinate(1, 1) -> Tree(5, Some(List(2)), Some(Vector(5, 1, 2)), Some(Vector(0)), Some(Vector(5, 3, 5))), Coordinate(1, 4) -> Tree(5, Some(List(3)), Some(Vector(3, 9, 0)), Some(Vector(0, 5, 5, 3)), None), Coordinate(4, 1) -> Tree(2, Some(List(2, 5, 5, 1)), None, Some(Vector(3)), Some(Vector(2, 9, 0))), Coordinate(1, 3) -> Tree(3, Some(List(3)), Some(Vector(5, 4, 9)), Some(Vector(0, 5, 5)), Some(Vector(5))), Coordinate(2, 0) -> Tree(3, Some(List(3, 0)), Some(Vector(7, 3)), None, Some(Vector(5, 3, 5, 3))), Coordinate(4, 2) -> Tree(2, Some(List(6, 5, 3, 3)), None, Some(Vector(3, 2)), Some(Vector(9, 0))), Coordinate(3, 1) -> Tree(1, Some(List(2, 5, 5)), Some(Vector(2)), Some(Vector(7)), Some(Vector(3, 4, 9))), Coordinate(3, 3) -> Tree(4, Some(List(3, 3, 5)), Some(Vector(9)), Some(Vector(7, 1, 3)), Some(Vector(9))), Coordinate(0, 2) -> Tree(6, None, Some(Vector(5, 3, 3, 2)), Some(Vector(3, 2)), Some(Vector(3, 3))), Coordinate(3, 0) -> Tree(7, Some(List(3, 0, 3)), Some(Vector(3)), None, Some(Vector(1, 3, 4, 9))), Coordinate(0, 1) -> Tree(2, None, Some(Vector(5, 5, 1, 2)), Some(Vector(3)), Some(Vector(6, 3, 3))), Coordinate(2, 4) -> Tree(3, Some(List(3, 5)), Some(Vector(9, 0)), Some(Vector(3, 5, 3, 5)), None), Coordinate(4, 4) -> Tree(0, Some(List(3, 5, 3, 9)), None, Some(Vector(3, 2, 2, 9)), None), Coordinate(2, 2) -> Tree(3, Some(List(6, 5)), Some(Vector(3, 2)), Some(Vector(3, 5)), Some(Vector(5, 3))), Coordinate(0, 0) -> Tree(3, None, Some(Vector(0, 3, 7, 3)), None, Some(Vector(2, 6, 3, 3))), Coordinate(1, 2) -> Tree(5, Some(List(6)), Some(Vector(3, 3, 2)), Some(Vector(0, 5)), Some(Vector(3, 5))), Coordinate(2, 3) -> Tree(5, Some(List(3, 3)), Some(Vector(4, 9)), Some(Vector(3, 5, 3)), Some(Vector(3))), Coordinate(0, 3) -> Tree(3, None, Some(Vector(3, 5, 4, 9)), Some(Vector(3, 2, 6)), Some(Vector(3))), Coordinate(3, 2) -> Tree(3, Some(List(6, 5, 3)), Some(Vector(2)), Some(Vector(7, 1)), Some(Vector(4, 9))))
    Eight.getBestScore(input) shouldBe 8
  }

  it should "do part 2" in {
    Eight.doPart2() shouldBe 527340
  }

}
