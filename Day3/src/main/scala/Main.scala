import scala.io.Source
import scala.util.matching.Regex.Match

@main def findPartsNumber: Unit =
  val lines = Source.fromFile("input").getLines.toList
  val matches = lines.map(x => "\\d+".r.findAllMatchIn(x))
  val res = matches
    .zipWithIndex
    .map((matches, row) => 
      matches.filter(checkNeighbors(lines, row, _))
    )
    .flatten
    .map(_.matched.toInt)
    .sum

  println(res)

def isSymbol(c: Char): Boolean =
  !"(\\d)|[.]".r.matches(c.toString)

def checkNeighbors(m: List[String], row: Int, mtch: Match): Boolean = 
  val start = mtch.start
  val end = mtch.end

  val aboveBelow = (start until end)
    .map(i => 
      (if row > 0 then isSymbol(m(row-1)(i)) else false) || 
      (if row < m.size - 1 then isSymbol(m(row+1)(i)) else false))
    .reduce((a,b) => a || b)

  val leftRight = (for {i <- List(row-1, row, row+1); j <- List(start-1, end)} yield (i,j))
    .map((i, j) => 
        if i > 0 && j > 0
        && i < m.size - 1 
        && j < m(0).size - 1
        then isSymbol(m(i)(j))
        else false
    )
    .reduce(_ || _)

  aboveBelow || leftRight


@main def findGearRatios: Unit =
  val lines = Source.fromFile("input").getLines.toList
  val matches = lines.map(x => "\\d+".r.findAllMatchIn(x).toList)
  val res = lines
    .zipWithIndex
    .map((line, rowNum) =>
      checkGears(line, matches, rowNum)
    ).sum

  println(res)

  
def checkGears(row: String, matches: List[List[Match]], rowNum: Int): Int =
  val gearIdxs: List[Int] = row.zipWithIndex.filter(_._1 == '*').map(_._2).toList
  val allGearRatios = gearIdxs.map(gearIdx =>
      val aboveAdjacents = if rowNum > 0 then matches(rowNum - 1)
        .filter(mtch =>
          mtch.end == gearIdx
          || mtch.start == gearIdx + 1
          || mtch.start <= gearIdx && mtch.end > gearIdx
        ).map(_.matched.toInt)
        else List()

      val belowAdjacents = if rowNum < matches.size - 1 then matches(rowNum + 1)
        .filter(mtch =>
          mtch.end == gearIdx
          || mtch.start == gearIdx + 1
          || mtch.start <= gearIdx && mtch.end > gearIdx
        ).map(_.matched.toInt)
        else List()

      val sameRowAdjacents = matches(rowNum)
        .filter(mtch =>
          mtch.start == gearIdx + 1
          || mtch.end == gearIdx
        ).map(_.matched.toInt)

      val allAdjacents = aboveAdjacents ++ belowAdjacents ++ sameRowAdjacents

      if allAdjacents.size == 2 then allAdjacents.product else 0
  )
  
  allGearRatios.sum

