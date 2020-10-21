use "ponytest"

actor Main is TestList

  new create(env: Env) => PonyTest(env, this)
  new make() => None

  fun tag tests(test: PonyTest) =>
    PartOneTests.make().tests(test)
    PartTwoTests.make().tests(test)


actor PartOneTests is TestList

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
    test(_SpreadsheetChecksumTest(DifferenceChecksumMode, numbers, 18))


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
  let _mode: ChecksumMode
  let _numbers: Array[Array[U64] val] val
  let _expected: U64

  new iso create(mode: ChecksumMode, numbers: Array[Array[U64] val] val, expected: U64) =>
    _mode = mode
    _numbers = numbers
    _expected = expected

  fun name(): String =>
    "Spreadsheet checksum test"

  fun apply(h: TestHelper) =>
    try
      let checksum = Spreadsheet(_numbers).checksum(_mode)?
      h.assert_eq[U64](_expected, checksum)
    else
      h.fail("Error")
    end


actor PartTwoTests is TestList

  new create(env: Env) => PonyTest(env, this)
  new make() => None

  fun tag tests(test: PonyTest) =>
    test(_RowEvenTest([5; 9; 2; 8], 4))
    test(_RowEvenTest([9; 4; 7; 3], 3))
    test(_RowEvenTest([3; 8; 6; 5], 2))

    let numbers: Array[Array[U64] val] val = [
      [5; 9; 2; 8]
      [9; 4; 7; 3]
      [3; 8; 6; 5]
    ]
    test(_SpreadsheetChecksumTest(DivisibleChecksumMode, numbers, 9))


class iso _RowEvenTest is UnitTest
  let _numbers: Array[U64] val
  let _expected: U64

  new iso create(numbers: Array[U64] val, expected: U64) =>
    _numbers = numbers
    _expected = expected

  fun name(): String =>
    "Even row division test"

  fun apply(h: TestHelper) =>
    try
      h.assert_eq[U64](_expected, Row(_numbers).even_division()?)
    else
      h.fail("No evenly divisible numbers found")
    end
