import scala.io.Source
import scala.math

@main def main: Unit =
  val lines = Source.fromFile("testinput").getLines
  val times = lines.take(1).toList.head
    .dropWhile(c => !"\\d".r.matches(c.toString))
    .trim
    .split(" +")
    .map(_.toInt)

  val distances = lines.take(1).toList.head
    .dropWhile(c => !"\\d".r.matches(c.toString))
    .toString
    .trim
    .split(" +")
    .map(_.toInt)

  val both = times.zip(distances)

  val res = both.map(tuple =>
    calculateErrorMargin(tuple._1, tuple._2)
  )
  .product

  println(res)


def calculateErrorMargin(time: Int, distance: Int): Int =
  val (start, end): (Double, Double) = solveQuadratic(-1, time, -(distance + 0.00001))
  math.floor(end).toInt - math.ceil(start).toInt + 1


def solveQuadratic(a: Int, b: Int, c: Double) =
  val plusMinus = math.sqrt(math.pow(b, 2) - 4*a*c)
  val firstPart = -b

  ((firstPart + plusMinus)/(2*a), (firstPart - plusMinus)/(2*a))


  
