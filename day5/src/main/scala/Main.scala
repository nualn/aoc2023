import scala.io.Source
import scala.collection.mutable.ListBuffer

@main def main: Unit =
  val lines = Source.fromFile("input").getLines()
  
  val seeds = lines.take(1)
    .toList
    .head
    .split(" ")
    .drop(1)
    .map(_.toLong)
    .grouped(2)
    .map(l => 
      (l(0), l(0)+l(1)-1)
    )

  val maps = ListBuffer[List[Array[Long]]]()
  while lines.hasNext do 

    val nextMap = lines.dropWhile(x => !".* map:".r.matches(x))
      .drop(1)
      .takeWhile(s => ("\\d+ \\d+ \\d+.*".r.matches(s)))
      .toList
      .map(line => line.split(" ").map(_.toLong))

    maps += nextMap

      
  val locationRanges = maps.foldLeft(seeds)((ranges, map) =>
    ranges.flatMap(range =>
      mapToNextRanges(map)(range)
    )
  )

  val minLocation = locationRanges.map(range => range._1).min

  println(minLocation)


def mapToNext(categories: List[Array[Long]])(source: Long): Long = 
  val potLine = categories.find(line =>
      source >= line(1)
      && source < line(1) + line(2)
  )
  
  potLine match
    case Some(line) => source - line(1) + line(0)
    case None => source


def mapToNextRanges(categories: List[Array[Long]])(range: (Long, Long)): List[(Long, Long)] = 
  val potLine = categories.find(line =>
      range._1 >= line(1)
      && range._1 < line(1) + line(2)
  )
  
  potLine match
    case Some(line) => 
      if range._2 < line(1) + line(2) then
        List((range._1 - line(1) + line(0), range._2 - line(1) + line(0)))
      else
        List((range._1 - line(1) + line(0), line(0) + line(2) - 1))
        ++ mapToNextRanges(categories)((line(1) + line(2), range._2))

    case None => 
      categories.find(line =>
        range._2 >= line(1)
      && range._2 < line(1) + line(2)
      ) match
        case Some(line) =>
          List((range._1, line(1)-1))
          ++ mapToNextRanges(categories)((line(1), range._2))

        case None => List(range)

  

