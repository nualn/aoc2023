import scala.io.Source

@main def main: Unit =
  val res = Source.fromFile("input")
    .getLines
    .filter(x => isPossible(x, 12, 13, 14))
    .map(x => "\\d+".r.findFirstIn(x).get.toInt)
    .sum

  println(res)


def isPossible(line: String, r: Int, g: Int, b: Int) =
  val cubeCounts = line.split(Array(':',';'))
    .drop(1)
    .map(_.trim.split(','))
    .flatten
    .map(x => 
        val arr = x.trim.split(' ')
        (arr(1), arr(0))
    )
    .groupBy(_._1)
    .map((color, arr) => (color, arr.map(_._2.toInt).max))
    .toMap
    .withDefaultValue(0)

  cubeCounts("red") <= r && cubeCounts("green") <= g && cubeCounts("blue") <= b


  

