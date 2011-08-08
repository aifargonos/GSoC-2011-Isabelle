
package subLaTeX



object Syntactic_Sugar
{
  
  import Lexer._
  
  
  def apply() =
  {
    syntactic_sugar
  }
  

  def comment: Parser[Token] =
  {// TODO .: Ignore or Invalid ???
    (
      is(Comment) ~
      rep(is_not(Newline)) ~
      is(Newline) ~
      rep(is(Space))
    ) ~>
    Lexer.token
  }.named("comment")

  def implicit_par: Parser[Token] =
  {
    is(Newline) ~ rep(is(Space)) ~ is(Newline) ^^
//    {"Command(\n\\par\n)"}// TODO .: this should call command \par and hence eat all white space after
    {_ => Command("\n\\par\n")}// TODO ... !!!
//    {_ => Macro("par")()}// TODO ...
  }.named("implicit par")
  
  
//  def syntactic_sugar: Parser[Any] =
  def syntactic_sugar: Parser[Token] =
  {// or preprocessing ??
    comment | implicit_par
  }.named("syntactic sugar")
  
  
}

