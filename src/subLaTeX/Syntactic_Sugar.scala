
package subLaTeX



object Syntactic_Sugar
{
  
  import Lexer._
  
  
  def apply(arg: Context) =
  {
    syntactic_sugar(arg)
  }
  
  
  def implicit_par(arg: Context): Parser[Context] =
  {
    ( is(Newline) ~ rep(is(Space)) ~ is(Newline) ) ~>
    {Macro("par")(arg)}
  }.named("implicit par")
  
  def comment(arg: Context): Parser[Context] =
  {// TODO .: Ignore or Invalid ???// TODO .: what if there should be an implicit par ??
    (
      is(Comment) ~
      rep(is_not(Newline)) ~
      is(Newline) ~
      rep(is(Space))
    ) ^^^
    {arg}
  }.named("comment")
  
  
  def syntactic_sugar(arg: Context): Parser[Context] =
  {// or preprocessing ??
    implicit_par(arg) | comment(arg)
  }.named("syntactic sugar")
  
  
}

