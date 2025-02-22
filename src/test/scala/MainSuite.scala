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

    val input2 = Input("123456789", 3)
    val current2 = "456789"
    assertEquals(input2.current(15), current2)
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

    val parser_failure2 = ParseFailure(Input("BA", 0))
    assertEquals(Parser.string("AAAA").parse("BA"), parser_failure2)
  }

  test("Parser Regex") {

    val parser_succeed1 = ParseSucceed("AB", Input("ABCD", 2))
    assertEquals(Parser.regex("A*B").parse("ABCD"), parser_succeed1)

    val parser_succeed2 = ParseSucceed("AAAB", Input("AAABCD", 4))
    assertEquals(Parser.regex("A*B").parse("AAABCD"), parser_succeed2)

    val parser_failure = ParseFailure(Input("ABCD", 0))
    assertEquals(Parser.regex("^C").parse("ABCD"), parser_failure)

    val parser_succeed3 = ParseSucceed("-512", Input("-512a", 4))
    assertEquals(Parser.regex("^-?[0-9]*").parse("-512a"), parser_succeed3)
  }

  test("FlatMap ParseResult") {
    val parser_succeed1 = ParseSucceed("AB", Input("ABCD", 2)).flatMap(
      a => ParseSucceed("BBB", Input("BBBAC", 3)).flatMap(
        b=> ParseSucceed(a + b, Input("BBBAC", 3))
      ))
    assertEquals(ParseSucceed("ABBBB", Input("BBBAC", 3)), parser_succeed1)
  }


  test("Map ParseResult") {
    val parser_succeed1 = ParseSucceed("12", Input("12a", 2)
    ).map(a => a.toInt)
    assertEquals(ParseSucceed(12, Input("12a", 2)), parser_succeed1)

    val parser_succeed2 =ParseSucceed("AAAB", Input("AAABCD", 4)
    ).map(a=> a+"A")
    assertEquals(ParseSucceed("AAABA",Input("AAABCD", 4)), parser_succeed2)
  }
  test("For Compréhension ParseResult") {
    val parser_succeed1 = ParseSucceed("AB", Input("ABCD", 2)).flatMap(
      a => ParseSucceed("BBB", Input("BBBAC", 3)).map(b => a+b))
    assertEquals(ParseSucceed("ABBBB", Input("BBBAC", 3)), parser_succeed1)
  }
  test("Map Parser") {
    // Create a string Parser then convert it to a int
    val parser_succeed1 = Parser.string("12").map(a=> a.toInt).parse("12a")
    assertEquals(ParseSucceed(12,Input("12a",2)), parser_succeed1)

    // Create a int Parser then convert it to a string
    val parser_succeed2 = Parser.int.map(a => a.toString).parse("12a")
    assertEquals(ParseSucceed("12", Input("12a", 2)), parser_succeed2)
  }
  test("test Oral, multiple -"){
    val arserInt = Parser.int.parse("-10000-1")
    assertEquals(ParseSucceed(-10000, Input("-10000-1", 6)), arserInt)
  }
  test("Trait ~"){
    val parser_succeed1 = ParseSucceed(("A","B"),Input("ABC",2))
    assertEquals((Parser.string("A") ~ Parser.string("B")).parse("ABC"), parser_succeed1)

    val parser_failure1 = ParseFailure(Input("1BC", 0))
    assertEquals((Parser.string("A") ~ Parser.string("B")).parse("1BC"), parser_failure1)

    val parser_failure2 = ParseFailure(Input("A10", 1))
    assertEquals((Parser.string("A") ~ Parser.string("B")).parse("A10"), parser_failure2)

  }

  test("Trait |"){
    val parser_succeed1 = ParseSucceed(Left("A"), Input("ABC", 1))
    assertEquals((Parser.string("A") | Parser.string("B")).parse("ABC"), parser_succeed1)

    val parser_succeed2 = ParseSucceed(Right("B"), Input("BAC", 1))
    assertEquals((Parser.string("A") | Parser.string("B")).parse("BAC"), parser_succeed2)

    val parser_succeed3 = ParseSucceed(Left(10), Input("10C", 2))
    assertEquals((Parser.int | Parser.string("A")).parse("10C"), parser_succeed3)

    val parser_succeed4 = ParseSucceed(Right(10), Input("10C", 2))
    assertEquals((Parser.string("A") | Parser.int).parse("10C"), parser_succeed4)

    val parser_failure1 = ParseFailure(Input("C", 0))
    assertEquals((Parser.string("A") | Parser.string("B")).parse("C"), parser_failure1)

    val parser_failure2 = ParseFailure(Input("10", 0))
    assertEquals((Parser.string("A") | Parser.string("B")).parse("10"), parser_failure2)
  }

  test("Trait ?"){
    val parser_succeed1 = ParseSucceed(Option.empty, Input("ABC", 0))
    assertEquals(Parser.int.?.parse("ABC"), parser_succeed1)

    val parser_succeed2 = ParseSucceed(Some(10), Input("10ABC", 2))
    assertEquals(Parser.int.?.parse("10ABC"), parser_succeed2)

    val parser_succeed3 = ParseSucceed(None, Input("10XYZ", 0))
    assertEquals(Parser.string("X").?.parse("10XYZ"), parser_succeed3)

    val parser_succeed4 = ParseSucceed(Some("A"), Input("AAA10", 1))
    assertEquals(Parser.string("A").?.parse("AAA10"), parser_succeed4)

    val parser_succeed5 = ParseSucceed(Some("AB"), Input("ABCD", 2))
    assertEquals(Parser.string("AB").?.parse("ABCD"), parser_succeed5)

  }

  test("Trait repeat"){
    val parser_succeed1 = ParseSucceed(List("A","A","A","A"), Input("AAAAB", 4))
    assertEquals(Parser.string("A").repeat.parse("AAAAB"), parser_succeed1)

    val parser_succeed2 = ParseSucceed(List(), Input("AAAA", 0))
    assertEquals(Parser.string("B").repeat.parse("AAAA"), parser_succeed2)

    val parser_succeed3 = ParseSucceed(List(), Input("AAAABBBB", 0))
    assertEquals(Parser.string("B").repeat.parse("AAAABBBB"), parser_succeed3)


    val parser_succeed4 = ParseSucceed(List("AB","AB","AB","AB"), Input("ABABABABAAAA", 8))
    assertEquals(Parser.string("AB").repeat.parse("ABABABABAAAA"), parser_succeed4)

  }
}


