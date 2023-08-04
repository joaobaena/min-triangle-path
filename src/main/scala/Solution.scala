import zio._

object Solution extends ZIOAppDefault {
  override def run =
    (for {
      args <- getArgs
      triangle <- loadTriangle(args)
    } yield ())
      .mapError{ e => println(s"Invalid input: ${e.getMessage}") }

  private def loadTriangle(args: Chunk[String]): Task[List[List[Int]]] = for {
    lines <- ZIO.attempt(args.head.split("\n").toList)
    linesWithIndex = lines.zipWithIndex
    triangle <- ZIO.foreach(linesWithIndex)({ case (line, lineIndex) => parseLine(line, lineIndex) })
  } yield triangle

  private def parseLine(line: String, lineIndex: Int): Task[List[Int]] =
    ZIO
      .attempt(line.split(" ").toList)
      .flatMap { nums =>
        if (nums.length == lineIndex + 1) ZIO.foreach(nums)(numStr => ZIO.attempt(numStr.toInt))
        else ZIO.fail(new IllegalArgumentException)
      }

}
