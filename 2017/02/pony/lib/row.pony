class val Row
  let _numbers: Array[U64] val

  new val create(numbers: Array[U64] val) =>
    _numbers = numbers

  fun difference(): U64 =>
    var max = U64.min_value()
    var min = U64.max_value()
    for n in _numbers.values() do
      if n > max then
        max = n
      end
      if n < min then
        min = n
      end
    end
    max - min

  fun even_division(): U64 ? =>
    for x in _numbers.values() do
      for y in _numbers.values() do
        if (x != y) and ((x % y) == 0) then
          return x / y
        end
      end
    end
    error
