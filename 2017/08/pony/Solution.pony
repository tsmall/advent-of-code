use "collections"
use "itertools"


actor Solution

  let _registers: Registers
  var _largest_value_encountered: I64

  new create() =>
    _registers = Registers
    _largest_value_encountered = 0

  be apply(m: Main, input: String val) =>
    try
      let instructions = Parse(input)?

      for instruction in instructions.values() do
        instruction(_registers)
        _check_for_largest_value_encountered()
      end

      m.report_part_one(_registers.largest_value().string())
      m.report_part_two(_largest_value_encountered.string())
    else
      m.report_error("Unable to parse input")
    end

  fun ref _check_for_largest_value_encountered() =>
    let largest = _registers.largest_value()
    if largest > _largest_value_encountered then
      _largest_value_encountered = largest
    end

primitive Parse

  fun apply(input: String): Array[Instruction] val ? =>
    let lines = recover val input.split("\n") end
    recover
      let instructions = Array[Instruction](lines.size())
      for line in lines.values() do
        let words = recover val line.split(" ") end
        let condition = _parse_condition(words)?
        let instruction = _parse_instruction(words, condition)?
        instructions.push(instruction)
      end
      instructions
    end

  fun _parse_condition(words: Array[String] box): Condition ? =>
    let register = words(4)?
    let amount = words(6)?.i64()?
    let constructor =
      match words(5)?
      | ">" => GT
      | ">=" => GTE
      | "<" => LT
      | "<=" => LTE
      | "==" => EQ
      | "!=" => NEQ
      else
        error
      end
    constructor(register, amount)

  fun _parse_instruction(words: Array[String] box, c: Condition): Instruction ? =>
    let register = words(0)?
    var amount = words(2)?.i64()?
    if words(1)? == "dec" then
      amount = amount * -1
    end
    Instruction(c, register, amount)


class Registers

  let _registers: Map[String, I64]

  new create() =>
    _registers = Map[String, I64]

  fun apply(register: String): I64 =>
    _registers.get_or_else(register, 0)

  fun ref adjust(register: String, amount: I64) =>
    _registers.upsert(register, amount, {(n, d) => n + d})

  fun largest_value(): I64 =>
    Iter[I64](_registers.values())
      .fold[I64](0, {(max, value) => max.max(value)})


class val Instruction

  let _condition: Condition
  let _register: String
  let _amount: I64

  new val create(c: Condition, register: String, amount: I64) =>
    _condition = c
    _register = register
    _amount = amount

  fun apply(r: Registers) =>
    if _condition(r) then
      r.adjust(_register, _amount)
    end


interface val ConditionFun
  fun val apply(value: I64, amount: I64): Bool


class val Condition

  let _register: String
  let _amount: I64
  let _fun: ConditionFun

  new val create(register: String, amount: I64, f: ConditionFun) =>
    _register = register
    _amount = amount
    _fun = f

  fun apply(r: Registers): Bool =>
    _fun(r(_register), _amount)


primitive GT

  fun apply(register: String, amount: I64): Condition =>
    Condition(register, amount, {(value: I64, amount: I64) => value > amount})


primitive GTE

  fun apply(register: String, amount: I64): Condition =>
    Condition(register, amount, {(value: I64, amount: I64) => value >= amount})


primitive LT

  fun apply(register: String, amount: I64): Condition =>
    Condition(register, amount, {(value: I64, amount: I64) => value < amount})


primitive LTE

  fun apply(register: String, amount: I64): Condition =>
    Condition(register, amount, {(value: I64, amount: I64) => value <= amount})


primitive EQ

  fun apply(register: String, amount: I64): Condition =>
    Condition(register, amount, {(value: I64, amount: I64) => value == amount})


primitive NEQ

  fun apply(register: String, amount: I64): Condition =>
    Condition(register, amount, {(value: I64, amount: I64) => value != amount})
