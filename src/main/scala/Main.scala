import ParseResult.*
import scala.util.matching.Regex
// Common interface for parsers
trait Parser[A]:
  /** parse with this and then parse with pb. */
  def ~[B](pb: => Parser[B]): Parser[(A, B)] = ???
  /** parse with this, or parse with pb if this fails. */
  def |[B](pb: Parser[B]): Parser[Either[A, B]] = ???
  /** try to parse with this. It does not fail if the parsing did not work. */
  def ? : Parser[Option[A]] = ???
  /** use this to parse multiple times, until it does not apply. */
  def repeat: Parser[List[A]] = ???
  /** convert the value output by the parser. */
  def map[B](f: A => B): Parser[B] = ???
  def flatMap[B](f: A => Parser[B]): Parser[B] = ???

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
  def Parser_Int(input: Input): ParseResult[Int]= {
    val regex = new Regex("^-?[0-9]+")
    val extractInteger = (regex  findAllIn input.data).mkString("")
    if (extractInteger.isEmpty)
      ParseFailure(input)
    else
      if (extractInteger.length == input.data.length)
        ParseSucceed(extractInteger.toInt, input.copy(offset = input.data.length-1))
        else
        ParseSucceed(extractInteger.toInt, input.copy(offset = extractInteger.length))
  }

  def int: Parser[Int] = createParser(Parser_Int)
  /** parse exactly the string s */
  def string(s: String): Parser[String] = createParser(???)
  /** parse according to a regular expression */
  def regex(r: String): Parser[String] = createParser(???)

// Result of parse
enum ParseResult[+A]:
  case ParseFailure(onInput: Input) extends ParseResult[Nothing]
  case ParseSucceed(value: A, remainingInput: Input) extends ParseResult[A]

  def map[B](f: A => B): ParseResult[B] = ???
  def flatMap[B](f: A => ParseResult[B]): ParseResult[B] = ???


@main
def _01_main(): Unit = {
  val x = new Regex("^-?[0-9]+")
  val myself = "-8912a45"
  println(Parser.int.parse("12"))
}
