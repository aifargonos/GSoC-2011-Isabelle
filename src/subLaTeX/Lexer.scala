
package subLaTeX



import scala.util.parsing.combinator.lexical.Scanners


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
  
  def escape: Parser[Elem] = acceptIf(Escape(_))(char_err("escape"))
  def begin: Parser[Elem] = acceptIf(Begin(_))(char_err("begin"))
  def end: Parser[Elem] = acceptIf(End(_))(char_err("end"))
  def math: Parser[Elem] = acceptIf(Math(_))(char_err("math"))
  def align: Parser[Elem] = acceptIf(Align(_))(char_err("align"))
  def newline: Parser[Elem] = acceptIf(Newline(_))(char_err("newline"))
  def param: Parser[Elem] = acceptIf(Param(_))(char_err("param"))
  def `super`: Parser[Elem] = acceptIf(Super(_))(char_err("super"))
  def sub: Parser[Elem] = acceptIf(Sub(_))(char_err("sub"))
  def ignore: Parser[Elem] = acceptIf(Ignore(_))(char_err("ignore"))
  def space: Parser[Elem] = acceptIf(Space(_))(char_err("space"))
  def letter: Parser[Elem] = acceptIf(Letter(_))(char_err("letter"))
  def other: Parser[Elem] = acceptIf(Other(_))(char_err("other"))
  def active: Parser[Elem] = acceptIf(Active(_))(char_err("active"))
  def comment: Parser[Elem] = acceptIf(Comment(_))(char_err("comment"))
  def invalid: Parser[Elem] = acceptIf(Invalid(_))(char_err("invalid"))
  
  
  
  def syntactic_sugar: Parser[Token] =
  {// or preprocessing ??
    Syntactic_Sugar()
  }.named("syntactic sugar")
  
  def command: Parser[Token] =
  {
    escape ~> (
      rep1(letter | elem('@')) ^^ {r => r.mkString} |
      acceptIf(!Letter(_))(char_err("non letter")) ^^ {_.toString}
    ) ^^
    {r => Command(r)}
  }.named("command")
  
  def white_space: Parser[Token] =
  {
    rep1(space | newline) ^^
    {r => White_Space(r.mkString)}
  }.named("white_space")
  
  def char: Parser[Token] =
  {
    (letter | other) ^^
    {r => Character(r)}
  }.named("char")
  
  def group: Parser[Token] =
  {
    begin ~> rep(token) <~ end ^^
    {r => Group(r)}
  }.named("group")
  
  
  def token: Parser[Token] =
  {
//    white_space | char | command | group
    syntactic_sugar | white_space | char | command | group
  }.named("token")
  
  

}
