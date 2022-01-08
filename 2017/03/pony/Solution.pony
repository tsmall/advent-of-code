actor Solution

  be apply(r: Reporter tag, input: String val) =>
    try
      let n = _parse(input)?
      let grid = MemoryGrid
      let part1 = grid.distance_from_location(n)
      let part2 = grid.first_value_larger_than(n)
      r.report(part1.string(), part2.string())
    else
      r.report_error("Unable to parse input: '" + input + "'")
    end

  fun _parse(input: String val): U64 ? =>
    input.trim(where to = input.size() - 1).u64()?
