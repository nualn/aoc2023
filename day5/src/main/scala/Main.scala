import scala.io.Source
import scala.collection.mutable.ListBuffer

@main def main: Unit =
  val lines = Source.fromFile("input").getLines()
  
  val seeds = lines.take(1).toList.head.split(" ").drop(1).map(_.toLong)
  

  val maps = ListBuffer[List[Array[Long]]]()
  while lines.hasNext do 

    val nextMap = lines.dropWhile(x => !".* map:".r.matches(x))
      .drop(1)
      .takeWhile(s => ("\\d+ \\d+ \\d+.*".r.matches(s)))
      .toList
      .map(line => line.split(" ").map(_.toLong))

    maps += nextMap

      
  val locations = seeds.map(seed =>
    maps.foldLeft(seed)((num, map) =>
      mapToNext(map)(num)
    )
  )

  println(locations.min)
    


def mapToNext(categories: List[Array[Long]])(source: Long): Long = 
  val potLine = categories.find(line =>
      source >= line(1)
      && source < line(1) + line(2)
  )
  
  potLine match
    case Some(line) => source - line(1) + line(0)
    case None => source
  


