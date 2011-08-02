
package subLaTeX



object Syntactic_Sugar
{
  
  import Lexer._
  
  
  def apply() =
  {
    syntactic_sugar
  }
  

  def comment: Parser[Token] =
  {
    (
      Lexer.comment ~
      rep(acceptIf(c => !Newline(c) && !Ignore(c))(char_err("non newline"))) ~
      newline ~
      rep(space)
    ) ~>
    Lexer.token
  }.named("comment")

  def implicit_par: Parser[Token] =
  {
    newline ~ rep(space) ~ newline >>
//    {"Command(\n\\par\n)"}// TODO .: this should call command \par and hence eat all white space after
//    {Control("\n\\par\n")}// TODO ...
    {_ => Macro("par")()}// TODO ...
  }.named("implicit par")
  
  
//  def syntactic_sugar: Parser[Any] =
  def syntactic_sugar: Parser[Token] =
  {// or preprocessing ??
    comment | implicit_par
  }.named("syntactic sugar")
  
  
}

