primitive Captcha

  fun one(input: String val): U64 =>
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

  fun two(input: String val): U64 =>
    var sum = U64(0)
    var i = USize(0)
    let half = input.size() / 2
    while i < input.size() do
      let c1 = try input(i)? else break end
      let c2 = try input((i + half) % input.size())? else break end
      if c1 == c2 then
        sum = sum + U64.from[U8](c1 - '0')
      end
      i = i + 1
    end
    sum

  fun _get_last(input: String val): U64 =>
    try
      U64.from[U8](input(input.size() - 1)? - '0')
    else
      0
    end
