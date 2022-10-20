
trait Parser[A]:
  /** parse with this and then parse with pb. */
  def ~[B](pb: => Parser[B]): Parser[(A, B)] = this.
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


val numPattern = "[0-9]+".r
val address = "123 Main Street Suite 101"
val match1 = numPattern.findFirstIn(address)



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

@main
def _01_main(): Unit = println(Input("123456789",2).remaining)