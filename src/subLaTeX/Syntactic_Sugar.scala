/*  Title:      src/subLaTeX/Syntactic_Sugar.scala
    Author:     aifargonos

Additional parser combinators for subLaTeX

that are called as first.
*/

package subLaTeX


/*
 * TODO .:
 *  make proper preprocessing as a CharSequenceReader doing substitutions
 *  and do implicit_par in it
 *  move comment to Lexer
 */

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
  {// TODO .: what if there should be an implicit par ?? .: si above
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

