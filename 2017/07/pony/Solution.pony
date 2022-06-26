use "collections"
use "itertools"

actor Solution

  be apply(r: Reporter tag, input: String val) =>
    try
      let parser = Parser(input)
      let base_program = parser()?
      r.report_part_one(base_program.name)

      let inspector = ProgramInspector(base_program)
      match inspector.unbalanced_program()
      | let p: Program box => r.report_part_two(p.corrected_weight().string())
      | None => r.report_part_two("Not found")
      end
    else
      r.report_error("Unable to parse input")
    end


class Parser

  let _lines: Array[String] val
  var _registry: Map[String, Program]
  var _base_program_name: String

  new create(input: String) =>
    _lines = recover val input.split("\n") end
    _registry = Map[String, Program]
    _base_program_name = ""

  fun ref apply(): Program ? =>
    if _base_program_name == "" then
      _parse_programs()?
      _populate_subprograms()?
    end
    _registry(_base_program_name)?

  fun ref _parse_programs() ? =>
    let all_program_names = Set[String]
    let stacked_program_names = Set[String]

    for line in _lines.values() do
      let words = recover val line.split(", ") end
      let program = _parse_program(words)?

      all_program_names.set(program.name)
      _registry.insert(program.name, program)

      for name in Iter[String](words.values()).skip(3) do
        if name != "" then
          stacked_program_names.set(name)
        end
      end
    end

    let unique = all_program_names.without(stacked_program_names)
    _base_program_name = unique.values().next()?

  fun ref _populate_subprograms() ? =>
    for line in _lines.values() do
      let words = recover val line.split(", ") end
      if words.size() > 2 then
        let base_program = _registry(words(0)?)?

        for name in Iter[String](words.values()).skip(3) do
          if name != "" then
            let subprogram = _registry(name)?
            base_program.add_subprogram(subprogram)
          end
        end

        for subprogram in base_program.subprograms() do
          for sibling in base_program.subprograms() do
            if sibling.name != subprogram.name then
              subprogram.add_sibling(sibling)
            end
          end
        end

        _registry.insert(base_program.name, base_program)
      end
    end

  fun _parse_program(words: Array[String] box): Program ? =>
    let name = words(0)?
    let weight = _parse_weight(words(1)?)?
    Program(name, weight)

  fun _parse_weight(word: String): I64 ? =>
    let numbers = word.trim(1, word.rfind(")")?.usize())
    numbers.i64()?


class Program

  let name: String
  let self_weight: I64
  let _subprograms: Array[Program]
  let _siblings: Array[Program]

  new create(name': String, weight': I64) =>
    name = name'
    self_weight = weight'
    _subprograms = Array[Program]
    _siblings = Array[Program]

  fun ref add_subprogram(p: Program) =>
    _subprograms.push(p)

  fun subprograms(): Iterator[this->Program] =>
    _subprograms.values()

  fun ref add_sibling(p: Program) =>
    _siblings.push(p)

  fun weight(): I64 =>
    self_weight + _subprogram_weight()

  fun _subprogram_weight(): I64 =>
    var sum = I64(0)
    for p in _subprograms.values() do
      sum = sum + p.weight()
    end
    sum

  fun corrected_weight(): I64 =>
    if is_balanced() then
      self_weight
    else
      let difference = _expected_weight() - weight()
      self_weight + difference
    end

  fun is_balanced(): Bool =>
    (not _has_siblings()) or _weight_matches_siblings()

  fun _has_siblings(): Bool =>
    _siblings.size() > 0

  fun _weight_matches_siblings(): Bool =>
    for sibling in _siblings.values() do
      if sibling.weight() == weight() then
        return true
      end
    end
    false

  fun _expected_weight(): I64 =>
    try
      _siblings(0)?.weight()
    else
      self_weight
    end


class ProgramInspector

  var _unbalanced_program: (Program box | None)
  var _programs_to_inspect: Iterator[Program box]

  new create(base_program: Program box) =>
    _unbalanced_program = None
    _programs_to_inspect = base_program.subprograms()

  fun ref unbalanced_program(): (Program box | None) =>
    if _unbalanced_program is None then
      _find_deepest_unbalanced_program()
    end
    _unbalanced_program

  fun ref _find_deepest_unbalanced_program() =>
    while _programs_to_inspect.has_next() do
      let next_depth = _inspect_programs()
      _programs_to_inspect = next_depth
    end

  fun ref _inspect_programs(): Iterator[Program box] =>
    let next_depth = Array[Iterator[Program box]]
    for program in _programs_to_inspect do
      if (not program.is_balanced()) then
        _unbalanced_program = program
      end
      next_depth.push(program.subprograms())
    end
    Iter[Program box].chain(next_depth.values())
