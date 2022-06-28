actor Solution

  be apply(m: Main, input: String val) =>
    let processor = StreamProcessor(input)
    try
      processor.process()?
      m.report_part_one(processor.score().string())
      m.report_part_two(processor.garbage_count().string())
    else
      let bytes = processor.bytes_processed()
      m.report_error("Failed after " + bytes.string() + " bytes")
    end


class StreamProcessor

  let _bytes: CountBytes
  var _byte: U8 = 0
  var _score: GroupScore = GroupScore
  var _in_garbage: Bool = false
  var _garbage_count: U64 = 0

  new create(stream: String) =>
    _bytes = CountBytes(stream)

  fun ref process() ? =>
    while _bytes.has_next() do
      _advance()?
      _process_byte()?
    end

  fun ref _advance() ? =>
    _byte = _bytes.next()?

  fun ref _process_byte() ? =>
    match _byte
    | '!' if _in_garbage => _advance()?
    | '<' if not _in_garbage => _in_garbage = true
    | '>' if _in_garbage => _in_garbage = false
    | '{' if not _in_garbage => _score.enter()
    | '}' if not _in_garbage => _score.exit()
    | let _: U8 if _in_garbage => _garbage_count = _garbage_count + 1
    end

  fun bytes_processed(): U64 =>
    _bytes.count()

  fun score(): U64 =>
    _score.value()

  fun garbage_count(): U64 =>
    _garbage_count


class CountBytes is Iterator[U8]

  let _bytes: Iterator[U8]
  var _count: U64 = 0

  new create(string: String) =>
    _bytes = string.values()

  fun ref has_next(): Bool =>
    _bytes.has_next()

  fun ref next(): U8 ? =>
    let result = _bytes.next()?
    _count = _count + 1
    result

  fun count(): U64 =>
    _count


class GroupScore

  var _score: U64 = 0
  var _depth: U64 = 0

  fun ref enter() =>
    _depth = _depth + 1

  fun ref exit() =>
    _score = _score + _depth
    _depth = _depth - 1

  fun value(): U64 =>
    _score
