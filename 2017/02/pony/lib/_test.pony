use "ponytest"

actor Main is TestList

  new create(env: Env) => PonyTest(env, this)
  new make() => None

  fun tag tests(test: PonyTest) =>
    test(_RowDifferenceTest([5; 1; 9; 5], 8))
    test(_RowDifferenceTest([7; 5; 3], 4))
    test(_RowDifferenceTest([2; 4; 6; 8], 6))

    let numbers: Array[Array[U64] val] val = [
      [5; 1; 9; 5]
      [7; 5; 3]
      [2; 4; 6; 8]
    ]
    test(_SpreadsheetChecksumTest(numbers, 18))


class iso _RowDifferenceTest is UnitTest
  let _numbers: Array[U64] val
  let _expected: U64

  new iso create(numbers: Array[U64] val, expected: U64) =>
    _numbers = numbers
    _expected = expected

  fun name(): String =>
    "Row difference test"

  fun apply(h: TestHelper) =>
    h.assert_eq[U64](_expected, Row(_numbers).difference())


class iso _SpreadsheetChecksumTest is UnitTest
  let _numbers: Array[Array[U64] val] val
  let _expected: U64

  new iso create(numbers: Array[Array[U64] val] val, expected: U64) =>
    _numbers = numbers
    _expected = expected

  fun name(): String =>
    "Spreadsheet checksum test"

  fun apply(h: TestHelper) =>
    h.assert_eq[U64](_expected, Spreadsheet(_numbers).checksum())
