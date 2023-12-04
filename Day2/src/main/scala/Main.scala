import scala.io.Source

@main def main: Unit =
  val res = Source.fromFile("input")
    .getLines
    .map(getPower)
    .sum

  println(res)


def getPower(line: String) =
   line.split(Array(':',';'))
    .drop(1)
    .map(_.trim.split(','))
    .flatten
    .map(x => 
        val arr = x.trim.split(' ')
        (arr(1), arr(0))
    )
    .groupBy(_._1)
    .map((color, arr) => arr.map(_._2.toInt).max)
    .product



  

