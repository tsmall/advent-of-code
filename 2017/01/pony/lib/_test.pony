use "ponytest"

actor Main is TestList

  new create(env: Env) => PonyTest(env, this)
  new make() => None

  fun tag tests(test: PonyTest) =>
    PartOneTests.make().tests(test)
    PartTwoTests.make().tests(test)


type CaptchaMethod is {(String val): U64} val


actor PartOneTests is TestList

  new make() => None

  fun tag tests(test: PonyTest) =>
    let method: CaptchaMethod = Captcha~one()
    test(_Example(where part = 1, input = "1122", expected = 3, method = method))
    test(_Example(where part = 1, input = "1111", expected = 4, method = method))
    test(_Example(where part = 1, input = "1234", expected = 0, method = method))
    test(_Example(where part = 1, input = "91212129", expected = 9, method = method))


actor PartTwoTests is TestList

  new make() => None

  fun tag tests(test: PonyTest) =>
    let method: CaptchaMethod = Captcha~two()
    test(_Example(where part = 2, input = "1212", expected = 6, method = method))
    test(_Example(where part = 2, input = "1221", expected = 0, method = method))
    test(_Example(where part = 2, input = "123425", expected = 4, method = method))
    test(_Example(where part = 2, input = "123123", expected = 12, method = method))
    test(_Example(where part = 2, input = "12131415", expected = 4, method = method))


class iso _Example is UnitTest
  let _part: U8
  let _input: String val
  let _expected: U64
  let _method: CaptchaMethod

  new iso create(part: U8, input: String val, expected: U64, method: CaptchaMethod) =>
    _part = part
    _input = input
    _expected = expected
    _method = method

  fun name(): String =>
    "Part " + _part.string() + " Example: " + _input

  fun apply(h: TestHelper) =>
    h.assert_eq[U64](_expected, _method(_input))
