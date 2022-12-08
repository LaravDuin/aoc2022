package seven

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SevenSpec extends AnyFlatSpec with Matchers {

  behavior of "Seven"

  it should "build the file system" in {
    val input = LazyList(
      "$ cd /",
      "$ ls",
      "dir a",
      "14848514 b.txt",
      "8504156 c.dat",
      "dir d",
      "$ cd a",
      "$ ls",
      "dir e",
      "29116 f",
      "2557 g",
      "62596 h.lst",
      "$ cd e",
      "$ ls",
      "584 i",
      "$ cd ..",
      "$ cd ..",
      "$ cd d",
      "$ ls",
      "4060174 j",
      "8033020 d.log",
      "5626152 d.ext",
      "7214296 k"
    )

    Seven.buildFileSystem(input, Directory.directoryMap) shouldBe Map("" -> Directory("",None,List("a", "d"),List(File("b.txt",14848514), File("c.dat",8504156))), "/a" -> Directory("/a",Some(""),List("e"),List(File("f",29116), File("g",2557), File("h.lst",62596))), "/d" -> Directory("/d",Some(""),List(),List(File("j",4060174), File("d.log",8033020), File("d.ext",5626152), File("k",7214296))), "/a/e" -> Directory("/a/e",Some("/a"),List(),List(File("i",584))))
  }

  it should "calculate the memory of a directory" in {
    val in = Map("" -> Directory("",None,List("a", "d"),List(File("b.txt",14848514), File("c.dat",8504156))), "/a" -> Directory("/a",Some(""),List("e"),List(File("f",29116), File("g",2557), File("h.lst",62596))), "/d" -> Directory("/d",Some(""),List(),List(File("j",4060174), File("d.log",8033020), File("d.ext",5626152), File("k",7214296))), "/a/e" -> Directory("/a/e",Some("/a"),List(),List(File("i",584))))
    val out = in.map{ case (k, v) => k -> v.directoryMemory(in) }
    out shouldBe Map("" -> 48381165, "/a" -> 94853, "/d" -> 24933642, "/a/e" -> 584)
  }

//  it should "do part 1" in {
//    Seven.doPart1() shouldBe 1443806
//  }

  it should "do part 2" in {
    Seven.doPart2() shouldBe 942298
  }
}
