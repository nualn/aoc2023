import scala.io.Source
import scala.math

@main def main: Unit =
  val lines = Source.fromFile("input").getLines
  val time = lines.take(1).toList.head
    .dropWhile(c => !"\\d".r.matches(c.toString))
    .trim
    .split(" +")
    .reduce((a,b) => a ++ b)
    .toLong

  val distance = lines.take(1).toList.head
    .dropWhile(c => !"\\d".r.matches(c.toString))
    .toString
    .trim
    .split(" +")
    .reduce((a,b) => a ++ b)
    .toLong

  val res = calculateErrorMargin(time, distance)

  println(res)


def calculateErrorMargin(time: Long, distance: Long): Long =
  val (start, end): (Double, Double) = solveQuadratic(-1, time, -(distance + 0.00001))
  math.floor(end).toLong - math.ceil(start).toLong + 1


def solveQuadratic(a: Long, b: Long, c: Double) =
  val plusMinus = math.sqrt(math.pow(b, 2) - 4*a*c)
  val firstPart = -b

  ((firstPart + plusMinus)/(2*a), (firstPart - plusMinus)/(2*a))


  
