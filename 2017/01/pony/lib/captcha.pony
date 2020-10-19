primitive Captcha

  fun apply(input: String val): U64 =>
    var sum = U64(0)
    var prev = _get_last(input)
    for char in input.values() do
      if (char < '0') or (char > '9') then
        continue
      end
      let n = U64.from[U8](char - '0')
      if n == prev then
        sum = sum + n
      end
      prev = n
    end
    sum

  fun _get_last(input: String val): U64 =>
    try
      U64.from[U8](input(input.size() - 1)? - '0')
    else
      0
    end
