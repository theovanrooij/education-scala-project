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
  def |[B](pb: Parser[B]): Parser[Either[A, B]] =
    Parser.createParser(input => {
      this.parse(input) match
        case ParseResult.ParseSucceed(value, input) => ParseResult.ParseSucceed(Left(value), input)
        case ParseResult.ParseFailure(input) => {
          pb.parse(input) match
            case ParseResult.ParseSucceed(value, input) => ParseResult.ParseSucceed(Right(value), input)
            case ParseResult.ParseFailure(input) => ParseResult.ParseFailure(input)
        }
    })

  /** try to parse with this. It does not fail if the parsing did not work. */
  def ? : Parser[Option[A]] =
    Parser.createParser(input => {
      this.parse(input) match
        case ParseResult.ParseSucceed(value, input) => ParseResult.ParseSucceed(Option(value), input)
        case ParseResult.ParseFailure(input) => ParseResult.ParseSucceed(Option.empty, input)
    })
  /** use this to parse multiple times, until it does not apply. */

  def duc(l: List[A], input: Input): List[A] = {
    this.parse(input) match
      case ParseResult.ParseSucceed(value, input2) => duc(l:+value, Input(input2.remaining))
      case ParseResult.ParseFailure(_) => l
  }
  def repeat: Parser[List[A]] =
    Parser.createParser(input => {
      this.parse(input) match
        case ParseResult.ParseSucceed(value, input) => {
          val l = duc(List(value), Input(input.remaining))
          ParseResult.ParseSucceed(l,input.copy(offset=l.length*l(0).toString.length))
        }
        case ParseResult.ParseFailure(input) => ParseResult.ParseSucceed(List(), input)
    })

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

case class Input(data: String, offset: Int = 0):
  def current(n: Int): String = {
    if (offset+n > data.length)
      this.remaining
    else
      data.substring(offset,offset+n)
  }
  def next(n: Int): Input = copy(offset = offset+n)
  def remaining: String = data.substring(offset)


object Parser:
  def createParser[A](f: Input => ParseResult[A]): Parser[A] = input => f(input)
  /** parse an integer. */
  //Modification après l'oral pour tester le fonctionnement
  def int: Parser[Int] = {
    createParser(input => parserRegex(input, "^-?[0-9]+").map(a => a.toInt))
  }

  def parserString(input: Input, s: String): ParseResult[String] = {
    if (s.length > input.remaining.length) return ParseFailure(input)
    val currentString = input.current(s.length)
    if (currentString == s)
      if (input.data.length > input.remaining.length)
        ParseSucceed(s, input.copy(offset = input.data.length - input.remaining.length + s.length))
      else if (s.length == input.data.length)
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
      case ParseResult.ParseSucceed(value, input) =>
        ParseSucceed(f(value),input)
      case ParseResult.ParseFailure(input) =>
        ParseResult.ParseFailure(input)

  def flatMap[B](f: A => ParseResult[B]): ParseResult[B] =
    this match
      case ParseResult.ParseSucceed(value,__) =>
        f(value)
      case ParseResult.ParseFailure(input) =>
        ParseResult.ParseFailure(input)


@main
def _01_main(): Unit = {
  println("Hello world")
  case class test(string:String,int:Int)

  val testParser: Parser[test] = (Parser.string("A")~Parser.int).map(
    (string, int)=>test(string,int))

  println(testParser.parse("A1"))

  println(Parser.int.parse("--12-a"))
}


