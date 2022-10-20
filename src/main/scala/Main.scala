import ParseResult.*

import scala.util.matching.Regex

// Common interface for parsers
trait Parser[A]:

  /** parse with this and then parse with pb. */
  def ~[B](pb: => Parser[B]): Parser[(A, B)] =
    for {
      a <- this
      b <- pb
    } yield (a, b)

  /** parse with this, or parse with pb if this fails. */
  def |[B](pb: Parser[B]): Parser[Either[A, B]] = ???
  /** try to parse with this. It does not fail if the parsing did not work. */
  def ? : Parser[Option[A]] = ???
  /** use this to parse multiple times, until it does not apply. */
  def repeat: Parser[List[A]] = ???

  /** convert the value output by the parser. */
  def map[B](f: A => B): Parser[B] = Parser.createParser(input => {this.parse(input).map(f)})
  def flatMap[B](f: A => Parser[B]): Parser[B] =
    Parser.createParser(input => {
    this.parse(input) match
      case ParseResult.ParseSucceed(value, input) => f(value).parse(input)
      case ParseResult.ParseFailure(input) => ParseResult.ParseFailure(input) }
  )

  final def parse(s: String): ParseResult[A] = parse(Input(s))
  def parse(input: Input): ParseResult[A]

// Input of a parser
// * data: represents the input string
// * offset: is the pointer on the string, that the parser increases during its processing
case class Input(data: String, offset: Int = 0):
  def current(n: Int): String = data.substring(offset,offset+n)
  def next(n: Int): Input = copy(offset = offset+n)
  def remaining: String = data.substring(offset)

// parser.string("A") tild parser.string("BCD")
// Input("ABCD", 0)
// first parser => Input("AABCD", 1)
// deuxième parser => input.current("BC".length)

object Parser:
  def createParser[A](f: Input => ParseResult[A]): Parser[A] = input => f(input)
  /** parse an integer. */
  def parserInt(input: Input): ParseResult[Int]= {

    val regex = new Regex("^-?[0-9]+")
    val extractInteger = (regex  findAllIn input.remaining).mkString("")
    if (extractInteger.isEmpty)
      ParseFailure(input)
    else
      if (extractInteger.length == input.data.length)
        ParseSucceed(extractInteger.toInt, input.copy(offset = input.data.length-1))
      else
        ParseSucceed(extractInteger.toInt, input.copy(offset = extractInteger.length))
  }

  def int: Parser[Int] = createParser(parserInt)

  def parserString(input: Input, s: String): ParseResult[String] = {
    val currentString = input.current(s.length)
    if (currentString == s)
      if (s.length == input.data.length)
        ParseSucceed(s, input.copy(offset = input.data.length - 1))
      else
        ParseSucceed(s, input.copy(offset = s.length))
    else
      ParseFailure(input)

  }

  /** parse exactly the string s */
  def string(s: String): Parser[String] = {
    createParser(input => parserString(input, s))
  }

  def parserRegex(input: Input, r: String): ParseResult[String] = {
    val regex : Regex = new Regex(r)
    val extractRegex = (regex findAllIn input.data).mkString("")
    if (extractRegex.isEmpty)
      ParseFailure(input)
    else if (extractRegex.length == input.data.length)
      ParseSucceed(extractRegex, input.copy(offset = input.data.length - 1))
    else
      ParseSucceed(extractRegex, input.copy(offset = extractRegex.length))
  }
  /** parse according to a regular expression */
  def regex(r: String): Parser[String] = {

    createParser(input => parserRegex(input,r))
  }

// Result of parse
enum ParseResult[+A]:
  case ParseFailure(onInput: Input) extends ParseResult[Nothing]
  case ParseSucceed(value: A, remainingInput: Input) extends ParseResult[A]

  def map[B](f: A => B): ParseResult[B] =
    this match
      case ParseResult.ParseSucceed(value, input) => ParseSucceed(f(value),input)
      case ParseResult.ParseFailure(input) => ParseResult.ParseFailure(input)
  def flatMap[B](f: A => ParseResult[B]): ParseResult[B] =
    this match
      case ParseResult.ParseSucceed(value,__) => f(value)
      case ParseResult.ParseFailure(input) => ParseResult.ParseFailure(input)


@main
def _01_main(): Unit = {
  println(Parser.regex("A*C").flatMap(a=> Parser.string("A*C")))
  println(ParseSucceed("AAAB", Input("AAABCD", 4)).flatMap(a =>
    ParseSucceed(a+"AAB", Input("AABCD", 3))))

  for{
    a <- ParseSucceed("AAAB", Input("AAABCD", 4))
    b <- ParseSucceed(a+"AAB", Input("AABCD", 4))
  } yield (b+"A")

  println((Parser.string("A") ~ Parser.string("B")).parse("ABC"))

}

/*Comment passer la valeur de string et regex
* besoin d'aide pour la combinaison de parser*/