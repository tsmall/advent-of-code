use "ponytest"

actor Main is TestList

  new create(env: Env) => PonyTest(env, this)
  new make() => None

  fun tag tests(test: PonyTest) =>
    test(_ExamplesTest(where input = "1122", expected = 3))
    test(_ExamplesTest(where input = "1111", expected = 4))
    test(_ExamplesTest(where input = "1234", expected = 0))
    test(_ExamplesTest(where input = "91212129", expected = 9))


class iso _ExamplesTest is UnitTest
  let _input: String val
  let _expected: U64

  new iso create(input: String val, expected: U64) =>
    _input = input
    _expected = expected

  fun name(): String =>
    "Example: " + _input

  fun apply(h: TestHelper) =>
    h.assert_eq[U64](_expected, Captcha(_input))
