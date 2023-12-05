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

  
