
package subLaTeX



object Syntactic_Sugar
{
  
  import Lexer._
  
  
//  def apply() =
//  {
//    syntactic_sugar
//  }
  def apply(arg: Context) =
  {
    syntactic_sugar(arg)
  }
  
  
//  def comment: Parser[Token] =
//  {// TODO .: Ignore or Invalid ???
//    (
//      is(Comment) ~
//      rep(is_not(Newline)) ~
//      is(Newline) ~
//      rep(is(Space))
//    ) ~>
//    Lexer.token
//  }.named("comment")
  def comment(arg: Context): Parser[Context] =
  {// TODO .: Ignore or Invalid ???
    (
      is(Comment) ~
      rep(is_not(Newline)) ~
      is(Newline) ~
      rep(is(Space))
    ) ^^^
    {arg}
  }.named("comment")
  
//  def implicit_par: Parser[Token] =
//  {
//    is(Newline) ~ rep(is(Space)) ~ is(Newline) ^^
////    {"Command(\n\\par\n)"}// TODO .: this should call command \par and hence eat all white space after
//    {_ => Command("\n\\par\n")}// TODO ... !!!
////    {_ => Macro("par")()}// TODO ...
//  }.named("implicit par")
  def implicit_par(arg: Context): Parser[Context] =
  {
    is(Newline) ~ rep(is(Space)) ~ is(Newline) ^^^
//    {"Command(\n\\par\n)"}// TODO .: this should call command \par and hence eat all white space after
    {arg + Command("\n\\par\n")}// TODO ... !!! call macro !!!
//    {_ => Macro("par")()}// TODO ...
  }.named("implicit par")
  
  
//  def syntactic_sugar: Parser[Token] =
//  {// or preprocessing ??
//    comment | implicit_par
//  }.named("syntactic sugar")
  def syntactic_sugar(arg: Context): Parser[Context] =
  {// or preprocessing ??
    comment(arg) | implicit_par(arg)
  }.named("syntactic sugar")
  
  
}

