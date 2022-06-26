// Advent of Code 2017
// Day 7: Recursive Circus


actor Main
  let _out: OutStream tag

  new create(env: Env) =>
    _out = env.out
    env.input(recover InputParser(this) end)

  be solve(input: String val) =>
    Solution(this, input)

  be report_part_one(answer: String) =>
    _out.print("Part 1: " + answer)

  be report_part_two(answer: String) =>
    _out.print("Part 2: " + answer)

  be report_error(message: String) =>
    _out.print("ERROR: " + message)

  be log(message: String) =>
    _out.print("LOG: " + message)


interface Reporter
  be report_part_one(answer: String)
  be report_part_two(answer: String)
  be report_error(message: String)
  be log(message: String)


class InputParser is InputNotify
  let _main: Main
  var _input: Array[U8] iso

  new create(main: Main) =>
    _main = main
    _input = recover Array[U8](2100) end

  fun ref apply(data: Array[U8 val] val) =>
    _input.append(data)

  fun ref dispose() =>
    let data = _input = recover Array[U8] end
    let s = String.from_array(consume data)
    _main.solve(s.trim(where to = s.size() - 1))
