
package subLaTeX



object Syntactic_Sugar
{
  
  import Lexer._
  
  
  def apply() =
  {
    syntactic_sugar
  }
  

  def new_line: Parser[String] =
  {
    "\n" | "\r\n"
  }.named("new_line")


//  def comment: Parser[Any] =
  def comment: Parser[Block] =
  {
    """%.*""".r ~ new_line ~ """\s*""".r ^^^
//    {"%comment"}// TODO .: make this produce no output (Nothing ?? Unit ???) .. or make it a command .. or produce a comment
    {new Compound(List(Simple(""))){override def toString = "%comment"}}// TODO ... or just Simple("") ??
  }.named("comment")

//  def implicit_par: Parser[Any] =
  def implicit_par: Parser[Block] =
  {
    new_line ~ """[\s&&[^\n]]*""".r ~ new_line ^^^
//    {"Command(\n\\par\n)"}// TODO .: this should call command \par and hence eat all white space after
    {new Compound(List(Simple(""))){override def toString = "Command(\n\\par\n)"}}// TODO ...
  }.named("implicit par")
  
  
//  def syntactic_sugar: Parser[Any] =
  def syntactic_sugar: Parser[Block] =
  {// or preprocessing ??
    comment | implicit_par
  }.named("syntactic sugar")
  
  
}

