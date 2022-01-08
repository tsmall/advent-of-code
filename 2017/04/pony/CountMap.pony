use "collections"


class CountMap is Equatable[CountMap]
  let _counts: Map[U8, U64] = Map[U8, U64]

  new from_string(s: String) =>
    for char in s.values() do
      inc(char)
    end

  fun eq(other: CountMap box): Bool =>
    if _counts.size() != other._counts.size() then
      return false
    end
    for (char, count) in _counts.pairs() do
      if other(char) != count then
        return false
      end
    end
    true

  fun apply(char: U8): U64 =>
    try
      _counts(char)?
    else
      0
    end

  fun ref inc(char: U8) =>
    _counts.upsert(char, 1, {(sum, n) => sum + n})
