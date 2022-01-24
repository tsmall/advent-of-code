use "files"

actor Main
  let _env: Env

  new create(env: Env) =>
    _env = env
    run(Solution)

  fun run(solution: Solution tag) =>
    try
      let input = read_input()?
      solution(this, input)
    else
      _env.out.print("ERROR: Couldn't read input")
    end

  fun read_input(): String val ? =>
    let path = FilePath(_env.root, "../input.txt")
    match OpenFile(path)
    | let file: File =>
      file.read_string(file.size())
    else
      error
    end

  be report_part_one(answer: String) =>
    _env.out.print("Part 1: " + answer)

  be report_part_two(answer: String) =>
    _env.out.print("Part 2: " + answer)

  be report_error(message: String) =>
    _env.out.print("ERROR: " + message)

  be log(data: Array[Stringable] val) =>
    for x in data.values() do
      _env.out.write(x.string())
      _env.out.write(" ")
    end
    _env.out.write("\n")


interface Reporter
  be report_part_one(answer: String)
  be report_part_two(answer: String)
  be report_error(message: String)
  be log(data: Array[Stringable] val)
