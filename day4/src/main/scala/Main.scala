import scala.io.Source

@main def main: Unit =
  val lines = Source.fromFile("input").getLines.toList
  val memo = new Array[Int](lines.size)

  val res = (0 until lines.size)
    .map(i =>
      dfs(lines, i, memo)
    ).sum

  println(res)


def dfs(lines: List[String], i: Int, memo: Array[Int]): Int =
  if memo(i) > 0 then return memo(i)

  val Array(winningNums, nums) = lines(i)
    .split(':')(1)
    .split('|')
    .map(_.trim)
    .map(_.split(" +"))

  val winning = winningNums.toSet

  val matches = nums.map(x =>
    if (winning.contains(x)) then
      1
    else
      0
  ).sum

  val res = (i+1 to Math.min(lines.size-1, i+matches)).map(x =>
    dfs(lines, x, memo)
  ).sum + 1

  memo(i) = res

  res

