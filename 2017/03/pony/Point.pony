use "collections"

class val Point is (Equatable[Point] & Hashable)
  let x: I64
  let y: I64

  new val create(x': I64, y': I64) =>
    x = x'
    y = y'

  new val origin() =>
    x = 0
    y = 0

  fun eq(that: Point box): Bool =>
    (x == that.x) and (y == that.y)

  fun hash(): USize =>
    // This algorithm is from "Effective Java".
    var result: USize = 17
    result = (31 * result) + x.hash()
    result = (31 * result) + y.hash()
    result

  fun moved(dx: I64, dy: I64): Point =>
    Point.create(x + dx, y + dy)

  fun distance(): U64 =>
    x.abs() + y.abs()

  fun neighbors(): Array[Point] val =>
    [
      Point(x - 1, y + 1); Point(x, y + 1); Point(x + 1, y + 1)
      Point(x - 1, y);                      Point(x + 1, y)
      Point(x - 1, y - 1); Point(x, y - 1); Point(x + 1, y - 1)
    ]
