use "lib"


actor Main
  let _env: Env

  new create(env: Env) =>
    _env = env
    env.input(recover InputParser(this) end)

  be solve(sheet: Spreadsheet) =>
    _env.out.print("Part 1: " + sheet.checksum().string())


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
    let string = String.from_array(consume data)
    let sheet = _parse(string.trim(where to = string.size() - 1))
    _main.solve(sheet)

  fun tag _parse(input: String val): Spreadsheet =>
    Spreadsheet(_parse_rows(input))

  fun tag _parse_rows(input: String val): Array[Array[U64] val] val =>
    let lines: Array[String] val = input.split("\n")
    recover val
      var rows: Array[Array[U64] val] = []
      for line in lines.values() do
        rows.push(_parse_row(line))
      end
      rows
    end

  fun tag _parse_row(line: String val): Array[U64] val =>
    recover val
      var numbers = Array[U64]
      for string in line.split("\t").values() do
        let number = try string.u64(where base = 10)? else 0 end
        numbers.push(number)
      end
      numbers
    end
