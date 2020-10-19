use "lib"

actor Main
  let _env: Env

  new create(env: Env) =>
    _env = env
    env.input(recover InputParser(this) end)

  be solve(input: String val) =>
    let answer = Captcha(input)
    _env.out.print("Part 1: " + answer.string())


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
