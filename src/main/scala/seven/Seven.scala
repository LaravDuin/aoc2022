package seven

import one.FileReader

import scala.util.matching.Regex

case class File(name: String, memory: Long)

case class Directory(path: String, parent: Option[String], subDirs: Seq[String], files: Seq[File]) {
  def addDir(nameDir: String): Directory = {
    Directory(path, parent, subDirs ++ Seq(nameDir), files)
  }

  def addFile(file: File): Directory = {
    Directory(path, parent, subDirs, files ++ Seq(file))
  }

  def fileSize: Long = files.map(_.memory).sum

  def directoryMemory: Map[String, Directory] => Long = dirMap => subDirs.map { dir =>
    dirMap(path + s"/$dir")
  }.map(_.directoryMemory(dirMap)).sum + fileSize
}

object Directory {
  val directoryMap: Map[String, Directory] = Map[String, Directory]("" -> Directory("", None, Seq.empty[String], Seq.empty[File]))
  val cdRegex: Regex = """^\$ cd ([a-zA-Z/.]*)""".r
  val listExpression = "$ ls"
  val dirOutput: Regex = "dir ([a-zA-Z]*)".r
  val fileOutput: Regex = raw"(\d*) ([a-zA-Z.]*)".r
  val totalDiskSpace = 70000000L
  val freeSpaceRequired = 30000000L
}

object Seven {

  val buildSystem: Map[String, Directory] = buildFileSystem(FileReader.lines.to(LazyList), Directory.directoryMap)

  def buildFileSystem(input: LazyList[String], start: Map[String, Directory]): Map[String, Directory] = {
    val (buildSystem, _) = input.tail.foldLeft[(Map[String, Directory], Directory)]((start, start(""))) { case ((acc, current), line) => line match {
      case Directory.cdRegex(location) if location == ".." => (acc, acc(current.parent.getOrElse("")))
      case Directory.cdRegex(location) =>
        val path = current.path + s"/$location"
        (acc, acc(path))
      case Directory.listExpression => (acc, current)
      case Directory.dirOutput(dirName) =>
        val newCurrent = current.addDir(dirName)
        val pathName = current.path + s"/$dirName"
        val newAcc = acc ++ Map(pathName -> Directory(pathName, Some(current.path), Seq.empty[String], Seq.empty[File])) ++ Map(current.path -> newCurrent)
        (newAcc, newCurrent)
      case Directory.fileOutput(memory, fileName) =>
        val newCurrent = current.addFile(File(fileName, memory.toLong))
        val newAcc = acc ++ Map(current.path -> newCurrent)
        (newAcc, newCurrent)
      case a =>
        println(s"Could not match: $a")
        (acc, current)
    }}
    buildSystem
  }

  def filterDirsOnMem(dir: Directory, memLimit: Long, dirMap: Map[String, Directory]): Boolean = {
    dir.directoryMemory(dirMap) <= memLimit
  }

  def doPart1(): Long = {
    println(buildSystem)
    buildSystem.values.filter { dir =>
      filterDirsOnMem(dir, 100000, buildSystem)
    }.map(_.directoryMemory(buildSystem)).sum
  }

  def getFreeSpace(input: Map[String, Directory]): Long = {
    val usedSpace = input("").directoryMemory(input)
    Directory.totalDiskSpace - usedSpace
  }

  def getAmountOfSpaceToFree(currentFreeSpace: Long): Long = {
    Directory.freeSpaceRequired - currentFreeSpace
  }

  def doPart2(): Long = {
    val toFreeUp: Long = getAmountOfSpaceToFree(getFreeSpace(buildSystem))
    buildSystem.values.map(_.directoryMemory(buildSystem)).filter(_ >= toFreeUp).toSeq.min
  }

}
