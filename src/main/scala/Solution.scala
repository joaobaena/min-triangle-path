import zio._

import scala.annotation.tailrec

object Solution extends ZIOAppDefault {
  override def run =
    (for {
      args <- getArgs
      triangle <- loadTriangle(args)
      solution = minimumPath(triangle)
    } yield println(s"Minimal path is: ${solution.mkString(" + ")} = ${solution.sum}"))
      .mapError { e => println(s"Invalid input: ${e.getMessage}") }

  private def loadTriangle(args: Chunk[String]): Task[Array[Array[Int]]] = for {
    lines <- ZIO.attempt(args.head.split("\n"))
    linesWithIndex = lines.zipWithIndex
    triangle <- ZIO.foreach(linesWithIndex)({ case (line, lineIndex) => parseLine(line, lineIndex) })
  } yield triangle

  private def parseLine(line: String, lineIndex: Int): Task[Array[Int]] =
    ZIO
      .attempt(line.split(" "))
      .flatMap { nums =>
        if (nums.length == lineIndex + 1) ZIO.foreach(nums)(numStr => ZIO.attempt(numStr.toInt))
        else ZIO.fail(new IllegalArgumentException)
      }


  private def minimumPath(triangle: Array[Array[Int]]): List[Int] = {
    val rows = triangle.length

    val original = triangle.map(_.clone())
    val accTriangle = (rows - 2 to 0 by -1).foldLeft(triangle) { (accTriangle, row) =>
      accTriangle(row).indices.foldLeft(accTriangle) { (accTriangle, col) =>
        val leftChild = accTriangle(row + 1)(col)
        val rightChild = accTriangle(row + 1)(col + 1)
        val minChild = Math.min(leftChild, rightChild)
        accTriangle(row)(col) += minChild

        accTriangle

      }

    }
    buildSolution(original, accTriangle)
  }

  private def buildSolution(original: Array[Array[Int]], accTriangle: Array[Array[Int]]): List[Int] = {
    val rows = original.length
    @tailrec
    def buildSolutionHelper(row: Int, lastColIndex: Int, acc: Array[Int]): Array[Int] = {
      if (row >= rows - 1) {
        acc(row) = original(row)(lastColIndex)
        acc
      } else {
        val leftChild = accTriangle(row + 1)(lastColIndex)
        val rightChild = accTriangle(row + 1)(lastColIndex + 1)
        val newLastColIndex = if (leftChild > rightChild) lastColIndex + 1 else lastColIndex
        acc(row) = original(row)(lastColIndex)
        buildSolutionHelper(row + 1, newLastColIndex, acc)
      }
    }
    val result: Array[Int] = buildSolutionHelper(0, 0, Array.ofDim[Int](rows))
    result.toList
  }


}
