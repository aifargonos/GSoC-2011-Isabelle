
package subLaTeX



import scala.util.parsing.combinator.lexical.Scanners


/* TODO
 *  whiteSpace !!!
 *  group is either one char or {body}
 *  syntacticSugar
 *  results
 */

object Lexer extends Scanners
{
  
  def rpt(s: String, i: Int): String =
  {// TODO .: debug
    import scala.collection.immutable.WrappedString
    new WrappedString(s)*i
  }

  var ind: Int = 0// TODO .: debug

  def alt[T](p: => Parser[T], q: => Parser[T]) = new Parser[T] {// TODO .: debug
    def apply(in: Input) = {

      println(rpt("\t",ind) + "alt")
      ind += 1

      println(rpt("\t",ind) + "trying p(" + p + ")")
      p(in) match {
        case s1 @ Success(r, _) =>
          println(rpt("\t",ind) + "p is successful with result: " + r)
          ind -= 1
          s1
        case failure =>
          println(rpt("\t",ind) + "p failed, trying q(" + q + ")")
          val ret = q(in)
          println(rpt("\t",ind) + "q results in: " + (ret match{case Success(r, _) => r case failure => "failure"}) )
          ind -= 1
          ret
      }
    }
  }
  
  
  
  type Token = subLaTeX.Token// TEST !!! will this work like this ???
  
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
    ( escape ~> (
      rep1(letter | elem('@')) ^^ {r => r.mkString} |
      acceptIf(!Letter(_))(char_err("non letter")) ^^ {_.toString}
    ) >> (Macro(_)()) ) ^^
    {r => r}
  }.named("command")
  
  def white_space: Parser[Token] =
  {
    rep1(space | newline) ^^
    {r => White_Space(r.mkString)}// TODO .: what should I do with the white space then ??
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
//    (white_space | char | command | group) ^^
    (syntactic_sugar | white_space | char | command | group) ^^
//    alt(alt(alt(alt(syntactic_sugar, white_space).named("then white_space"), char).named("then char"), command).named("then command"), group).named("then group") ^^
    {r => r}
  }.named("lexem")
  
  
  def body: Parser[List[Token]] =
  {
    rep(token) ^^
    {r => r}
  }.named("body")
  


}
