import scala.io.StdIn


@main def solution =
  val input = StdIn.readLine()
  val floors = input.toCharArray.scanLeft(0)(move)

  val finalFloor = floors.last
  val firstBasementIndex = floors.indexWhere(_ < 0)

  println(s"Part 1: $finalFloor")
  println(s"Part 2: $firstBasementIndex")


def move(floor: Int, dir: Char): Int =
  dir match
    case '(' => floor + 1
    case ')' => floor - 1
