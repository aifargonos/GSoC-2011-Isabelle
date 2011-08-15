
package subLaTeX



object Syntactic_Sugar
{
  
  import Lexer._
  
  
  def apply(arg: Context) =
  {
    syntactic_sugar(arg)
  }
  
  
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
  
  def implicit_par(arg: Context): Parser[Context] =
  {
    ( is(Newline) ~ rep(is(Space)) ~ is(Newline) ) ~>
    {Macro("par")(arg)}
  }.named("implicit par")
  
  
  def syntactic_sugar(arg: Context): Parser[Context] =
  {// or preprocessing ??
    comment(arg) | implicit_par(arg)
  }.named("syntactic sugar")
  
  
}

