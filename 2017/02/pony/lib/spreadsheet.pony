type ChecksumMode is (DifferenceChecksumMode | DivisibleChecksumMode)

primitive DifferenceChecksumMode
primitive DivisibleChecksumMode


class val Spreadsheet
  let _rows: Array[Row] val

  new val create(numbers: Array[Array[U64] val] val) =>
    _rows = _create_rows(numbers)

  fun tag _create_rows(numbers: Array[Array[U64] val] val): Array[Row] val =>
    recover val
      var rows = Array[Row](numbers.size())
      for row_numbers in numbers.values() do
        rows.push(Row(row_numbers))
      end
      rows
    end

  fun val checksum(mode: ChecksumMode): U64 ? =>
    var sum = U64(0)
    for row in _rows.values() do
      sum = sum + match mode
      | DifferenceChecksumMode => row.difference()
      | DivisibleChecksumMode => row.even_division()?
      end
    end
    sum
