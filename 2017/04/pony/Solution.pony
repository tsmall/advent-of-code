use "collections"


actor Solution

  be apply(r: Reporter tag, input: String val) =>
    let passphrases = _parse(input)
    let valid_literal = _count_valid(passphrases, LiteralValidation)
    let valid_anagram = _count_valid(passphrases, AnagramValidation)
    r.report(valid_literal.string(), valid_anagram.string())

  fun _count_valid(passphrases: Array[Passphrase] box, alg: ValidationAlgorithm): U64 =>
    var count: U64 = 0
    for passphrase in passphrases.values() do
      if passphrase.is_valid(alg) then
        count = count + 1
      end
    end
    count

  fun _parse(input: String val): Array[Passphrase] val =>
    let result = recover trn Array[Passphrase] end
    for line in input.split("\n").values() do
      if line.size() > 0 then
        let words = recover val line.split(" ") end
        result.push(Passphrase(words))
      end
    end
    consume val result


class val Passphrase
  let words: Array[String] val

  new val create(words': Array[String] val) =>
    words = words'

  fun is_valid(alg: ValidationAlgorithm box): Bool =>
    alg(words)


trait val ValidationAlgorithm

  fun apply(words: Array[String] box): Bool

  fun any_same[A: Equatable[A] #read](xs: Array[A] box): Bool =>
    for i in xs.keys() do
      for j in Range(i + 1, xs.size()) do
        try
          if xs(i)? == xs(j)? then
            return true
          end
        end
      end
    end
    false


primitive LiteralValidation is ValidationAlgorithm

  fun apply(words: Array[String] box): Bool =>
    not any_same[String](words)


primitive AnagramValidation is ValidationAlgorithm

  fun apply(words: Array[String] box): Bool =>
    let counts = _to_counts(words)
    not any_same[CountMap val](counts)

  fun _to_counts(words: Array[String] box): Array[CountMap val] val =>
    let counts = recover trn Array[CountMap val].create(words.size()) end
    for word in words.values() do
      counts.push(recover val CountMap.from_string(word) end)
    end
    consume counts
