
package subLaTeX



import scala.collection.immutable.Queue
import scala.util.parsing.combinator.lexical.Scanners
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.input.Reader


/* DONE
 *  whiteSpace !!!
 *  group is either one char or {body}
 *  syntacticSugar
 *  results = tokens
 */

object Lexer extends Scanners
{
  
  
  
  type Token = subLaTeX.Token
  
  def whitespace: Parser[Any] = success(())
  
  def errorToken(msg: String): Token = ErrorToken(msg)
  
  
  
  def char_err(expected: String)(e: Elem) = expected + " expected but " + e + " found"
  
//  val escape: Parser[Elem] = acceptIf(Escape(_))(char_err("escape"))
//  val begin: Parser[Elem] = acceptIf(Begin(_))(char_err("begin"))
//  val end: Parser[Elem] = acceptIf(End(_))(char_err("end"))
//  val math: Parser[Elem] = acceptIf(Math(_))(char_err("math"))
//  val align: Parser[Elem] = acceptIf(Align(_))(char_err("align"))
//  val newline: Parser[Elem] = acceptIf(Newline(_))(char_err("newline"))
//  val param: Parser[Elem] = acceptIf(Param(_))(char_err("param"))
//  val `super`: Parser[Elem] = acceptIf(Super(_))(char_err("super"))
//  val sub: Parser[Elem] = acceptIf(Sub(_))(char_err("sub"))
//  val ignore: Parser[Elem] = acceptIf(Ignore(_))(char_err("ignore"))
//  val space: Parser[Elem] = acceptIf(Space(_))(char_err("space"))
//  val letter: Parser[Elem] = acceptIf(Letter(_))(char_err("letter"))
//  val other: Parser[Elem] = acceptIf(Other(_))(char_err("other"))
//  val active: Parser[Elem] = acceptIf(Active(_))(char_err("active"))
//  val comment: Parser[Elem] = acceptIf(Comment(_))(char_err("comment"))
//  val invalid: Parser[Elem] = acceptIf(Invalid(_))(char_err("invalid"))
  
  // TODO .. do propper character ignoration (Ignore)
  
  def is(cc: CatCode): Parser[Elem] = acceptIf(cc(_))(char_err(cc.toString))
  
  def is_not(cc: CatCode): Parser[Elem] =
    acceptIf(c => !cc(c) && !Ignore(c) && !Invalid(c))(char_err("non " + cc.toString))
  
  
  
  def syntactic_sugar: Parser[Token] =
  {// or preprocessing ??
    Syntactic_Sugar()
  }.named("syntactic sugar")
  
  def command: Parser[Token] =
  {
    is(Escape) ~> (
      rep1(is(Letter) | elem('@')) ^^ {r => r.mkString} |
//      acceptIf(c => !Letter(c) && !Ignore(c) && !Invalid(c))(char_err("non letter")) ^^ {_.toString}// TODO .: non()
      is_not(Letter) ^^ {_.toString}
    ) ^^
    {r => Command(r)}
  }.named("command")
  
  def white_space: Parser[Token] =
  {
    rep1(is(Space) | is(Newline)) ^^
    {r => White_Space(r.mkString)}
  }.named("white_space")
  
  def character: Parser[Token] =
  {
    (is(Letter) | is(Other)) ^^
    {r => Character(r)}
  }.named("char")
  
  def group: Parser[Token] =
  {
    is(Begin) ~> rep(token) <~ is(End) ^^
    {r => Group(r)}
  }.named("group")
  
  
  def token: Parser[Token] =
  {
//    white_space | char | command | group
    syntactic_sugar | white_space | character | command | group
  }.named("token")
  
  
  
  /* \section{syntactic level} */
  
  type Context = Queue[Token]
  
  
  
  /**
   * TODO...
   * parsers passed to this function must combine their results on their own !!!
   *
   */
  def rep_arg[T](arg: T)(pf: T => Parser[T]): Parser[T] =
  {
    pf(arg) >> {a: T => rep_arg(a)(pf)} | success(arg)
  }


  
  def syntactic_sugar(arg: Context): Parser[Context] =
  {// or preprocessing ??
    syntactic_sugar ^^
    {r => arg + r}
  }//TODO .: .named("syntactic sugar")
  
  def command(arg: Context): Parser[Context] =
  {
//    command ^^
//    {r => arg + r}// TODO .: Macro !!!
    command >>
    {r =>
      val Command(cmd) = r
      Macro(cmd)(arg)// FIXME !!! see the output !!!
    }
  }
  
  def character(arg: Context): Parser[Context] =
  {
    character ^^
    {r => arg + r}
  }
  
  def white_space(arg: Context): Parser[Context] =
  {
    white_space ^^
    {r => arg + r}
  }

  def begin(arg: Context): Parser[Context] = is(Begin) ^^^ {arg + Command("BEGIN")}// TODO .: command
  def end(arg: Context): Parser[Context] = is(End) ^^^ {arg + Command("END")}// TODO .: command
  def group(arg: Context): Parser[Context] =
  {
    begin(arg) >> body >> end
  }
  
  def body(arg: Context): Parser[Context] =
  {
    rep_arg(arg){arg =>
      syntactic_sugar(arg) | command(arg) | character(arg) | white_space(arg) | group(arg)
    }
  }
  
  
  // TODO .: all these functions :.
  def parseAll(in: Reader[Char]) =
  {
    phrase(body(Queue.empty[Token]))(in.asInstanceOf[Reader[Elem]])
  }
  
  
  
}
