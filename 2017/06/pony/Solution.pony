use "collections"


actor Solution

  be apply(r: Reporter tag, input: String val) =>
    let bank = MemoryBank(_parse(input))
    try
      r.report_part_one(_run_until_repeat_detected(bank)?.string())
      r.report_part_two(_run_until_repeat_detected(bank)?.string())
    else
      r.report_error("Computation failed")
    end

  fun _run_until_repeat_detected(bank: MemoryBank ref): U64 ? =>
    var cycle = U64(0)
    let configurations_seen = Set[U64]
    var hash = bank.hash()
    while not configurations_seen.contains(hash) do
      configurations_seen.set(hash)
      hash = bank.redistribute()?
      cycle = cycle + 1
    end
    cycle

  fun _parse(input: String val): Array[U64] ref =>
    let numbers = Array[U64]()
    var number: U64 = 0
    for c in input.values() do
      if (c == '\n') or (c == '\t') then
        numbers.push(number)
        number = 0
      elseif (c >= '0') and (c <= '9') then
        number = (number * 10) + (c - '0').u64()
      end
    end
    numbers


class MemoryBank
  let _banks: Array[U64] ref

  new create(banks: Array[U64] ref) =>
    _banks = banks

  fun ref redistribute(): U64 ? =>
    let most_full_bank_index: USize = _find_most_full_bank_index()
    let blocks_to_redistribute: U64 = _clear_bank(most_full_bank_index)?
    _distribute_blocks(where
      count = blocks_to_redistribute,
      starting_bank = _next_bank_index(most_full_bank_index)
    )?
    hash()

  fun box _find_most_full_bank_index(): USize =>
    var max_index: USize = 0
    var max_blocks: U64 = 0
    for (i, blocks) in _banks.pairs() do
      if blocks > max_blocks then
        max_index = i
        max_blocks = blocks
      end
    end
    max_index

  fun ref _clear_bank(index: USize): U64 ? =>
    _banks(index)? = 0

  fun box _next_bank_index(starting_bank: USize): USize =>
    (starting_bank + 1) % _banks.size()

  fun ref _distribute_blocks(count: U64, starting_bank: USize) ? =>
    var remaining: U64 = count
    var index: USize = starting_bank
    while remaining > 0 do
      _banks(index)? = _banks(index)? + 1
      remaining = remaining - 1
      index = _next_bank_index(index)
    end

  fun box hash(): U64 =>
    var n: U64 = 0
    for blocks in _banks.values() do
      n = (n * 31) + blocks
    end
    n
