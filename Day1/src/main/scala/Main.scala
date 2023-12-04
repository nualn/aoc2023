import scala.io.Source

@main def main: Unit =
  val filename = "input"
  println(getCalibrations(filename))

def getCalibrations(filename: String): Int =
  val bufferedSource = Source.fromFile(filename)
  val res = bufferedSource.getLines()
    .map(findNum)
    //.tapEach(println)
    .reduce((a: Int, b: Int) => a + b)
  bufferedSource.close
  res

def findNum(line: String): Int = 
  val matches = regex.findAllMatchIn(line).map(m => m.group(1)).toList
  val first = getNum(matches.head)
  val last = getNum(matches.last)
  return (first + last).toInt

def getNum(s: String) =
  s match {
    case "one" => "1" 
    case "two" => "2"
    case "three" => "3"
    case "four" => "4"
    case "five" => "5"
    case "six" => "6"
    case "seven" => "7"
    case "eight" => "8"
    case "nine" => "9"
    case _ => s
  }

val strings = List("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

val keys = strings.reduce((a, b) => a + "|" + b)

val regex = s"(?=(\\d|$keys))".r
