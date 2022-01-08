use "collections"


class MemoryGrid
  let _cells: Map[Point, MemoryCell] ref

  new create() =>
    _cells = Map[Point, MemoryCell].create(where prealloc = 200_000)
    let origin = MemoryCell._origin()
    _cells(origin.point) = origin

  fun ref cells(): Iterator[MemoryCell] ref^ =>
    _MemoryIterator._create(_cells)

  fun ref distance_from_location(location: U64): (U64 | None) =>
    for cell in cells() do
      if cell.location == location then
        return cell.distance()
      end
    end
    None

  fun ref first_value_larger_than(value: U64): (U64 | None) =>
    for cell in cells() do
      if cell.value > value then
        return cell.value
      end
    end
    None


class val MemoryCell
  let point: Point
  let value: U64
  let location: U64

  new val _create(point': Point, value': U64, location': U64) =>
    point = point'
    value = value'
    location = location'

  new val _origin() =>
    point = Point.origin()
    value = 1
    location = 1

  fun distance(): U64 =>
    point.distance()


class _MemoryIterator is Iterator[MemoryCell]
  let _cells: Map[Point, MemoryCell] ref
  var _curr: MemoryCell
  var _prev: MemoryCell
  let _points: _PointIterator

  new _create(cells: Map[Point, MemoryCell] ref) =>
    _cells = cells
    _curr = MemoryCell._origin()
    _prev = _curr
    _points = _PointIterator

  fun ref has_next(): Bool val =>
    _curr.location < (U64.max_value() - 1)

  fun ref next(): MemoryCell ? =>
    if not _points.has_next() then error end
    let next_point = _points.next()
    if _cells.contains(next_point) then
      _prev = _curr = _cells(next_point)?
    else
      let next_cell = _generate_cell(next_point)
      _cells(next_point) = next_cell
      _prev = _curr = next_cell
    end
    _prev

  fun _generate_cell(point: Point): MemoryCell =>
    MemoryCell._create(
      point,
      _value(point),
      _curr.location + 1
    )

  fun _value(point: Point val): U64 =>
    var sum: U64 = 0
    for neighbor in point.neighbors().values() do
      try
        let cell = _cells(neighbor)?
        sum = sum + cell.value
      end
    end
    sum


class _PointIterator is Iterator[Point]
  var _dx: I64 = 1
  var _dy: I64 = 0
  var _tx: I64 = 1
  var _ty: I64 = 0
  var _curr: Point = Point.origin()
  var _target: Point = Point(_tx, _ty)

  fun has_next(): Bool =>
    true

  fun ref next(): Point =>
    let next_point = _curr.moved(_dx, _dy)

    if next_point == _target then
      if _tx == 0 then
        let value: I64 = _ty.abs().i64() + 1
        let sign: I64 = if _ty < 0 then 1 else -1 end
        _tx = value * sign
        _dx = sign
        _ty = 0
        _dy = 0
      else
        _ty = _tx = 0
        _dy = _dx = 0
      end

      _target = _target.moved(_tx, _ty)
    end

    _curr = next_point
