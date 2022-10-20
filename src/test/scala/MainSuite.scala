// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
import ParseResult.*

class MainSuite extends munit.FunSuite {
  test("example test that succeeds") {
    val obtained = 42
    val expected = 42
    assertEquals(obtained, expected)
  }

  test("Inputs remaining"){
    val input = Input("123456789",3)
    val remaining = "456789"
    assertEquals(input.remaining,remaining)
  }
  test("Inputs current") {
    val input = Input("123456789", 3)
    val current = "456"
    assertEquals(input.current(3), current)
  }
  test("Inputs next") {
    val input = Input("123456789", 3)
    val next = Input("123456789", 5)
    assertEquals(input.next(2), next)
  }
  test("Parser Int") {


    val parser_succeed1 = ParseSucceed(12,Input("12a",2))
    assertEquals(Parser.int.parse("12a"), parser_succeed1)

    val parser_succeed2 = ParseSucceed(-12,Input("-12a",3))
    assertEquals(Parser.int.parse("-12a"), parser_succeed2)

    val parser_succeed3 = ParseSucceed(12,Input("12-a",2))
    assertEquals(Parser.int.parse("12-a"), parser_succeed3)

    val parser_succeed4 = ParseSucceed(12, Input("12", 1))
    assertEquals(Parser.int.parse("12"), parser_succeed4)
    
    val parser_failure = ParseFailure(Input("a12",0))
    assertEquals(Parser.int.parse("a12"), parser_failure)

    val parser_failure2 = ParseFailure(Input("-a12", 0))
    assertEquals(Parser.int.parse("-a12"), parser_failure2)

    val parser_failure3 = ParseFailure(Input("--12", 0))
    assertEquals(Parser.int.parse("--12"), parser_failure3)

    val parser_succeed5 = ParseSucceed(-12,Input("-12-a", 3))
    assertEquals(Parser.int.parse("-12-a"), parser_succeed5)
  }

  test("Parser String") {


    val parser_succeed1 = ParseSucceed("A", Input("AB", 1))
    assertEquals(Parser.string("A").parse("AB"), parser_succeed1)

    val parser_succeed2 = ParseSucceed("A", Input("AAB", 1))
    assertEquals(Parser.string("A").parse("AAB"), parser_succeed2)

    val parser_succeed3 = ParseSucceed("AB", Input("ABCD", 2))
    assertEquals(Parser.string("AB").parse("ABCD"), parser_succeed3)


    val parser_failure = ParseFailure(Input("BA", 0))
    assertEquals(Parser.string("A").parse("BA"), parser_failure)
  }

  test("Parser Regex") {

    val parser_succeed1 = ParseSucceed("AB", Input("ABCD", 2))
    assertEquals(Parser.regex("A*B").parse("ABCD"), parser_succeed1)

    val parser_succeed2 = ParseSucceed("AAAB", Input("AAABCD", 4))
    assertEquals(Parser.regex("A*B").parse("AAABCD"), parser_succeed2)

    val parser_failure = ParseFailure(Input("ABCD", 0))
    assertEquals(Parser.regex("^C").parse("ABCD"), parser_failure)
  }

}
