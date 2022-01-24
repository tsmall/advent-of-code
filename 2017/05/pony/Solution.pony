actor Solution

  be apply(r: Reporter tag, input: String val) =>
    let jump_offsets =
      try
        _parse_jump_offsets(input)?
      else
        r.report_error("Unable to parse input")
        return
      end

    let simple_offset_update = {(offset: I64): I64 => offset + 1}
    let strange_offset_update = {(offset: I64): I64 => if offset >= 3 then offset - 1 else offset + 1 end}

    let answer_one =
      try
        let step_count = _count_steps_until_done(jump_offsets, simple_offset_update)?
        step_count.string()
      else
        "ERROR"
      end

    let answer_two =
      try
        let step_count = _count_steps_until_done(jump_offsets, strange_offset_update)?
        step_count.string()
      else
        "ERROR"
      end

    r.report(answer_one, answer_two)

  fun _count_steps_until_done(jump_offsets: Array[I64] val, offset_update_fun: {(I64): I64} val): U64 ? =>
    let current_offsets: Array[I64] ref = jump_offsets.clone()
    var jump_index: USize = 0
    var step_count: U64 = 0

    while jump_index < current_offsets.size() do
      let jump_offset = current_offsets(jump_index)?
      current_offsets(jump_index)? = offset_update_fun(jump_offset)
      jump_index = (jump_index.i64() + jump_offset).usize()
      step_count = step_count + 1
    end

    step_count

  fun _parse_jump_offsets(input: String val): Array[I64] val ? =>
    let jump_offsets = recover trn Array[I64] end
    for line in input.split("\n").values() do
      if line.size() > 0 then
        let offset = line.i64()?
        jump_offsets.push(offset)
      end
    end

    consume jump_offsets
