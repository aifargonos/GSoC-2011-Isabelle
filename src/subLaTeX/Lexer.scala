
package subLaTeX



import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.RegexParsers


/* TODO
 *  whiteSpace !!!
 *  group is either one char or {body}
 *  syntacticSugar
 *  results
 */

object Lexer extends RegexParsers
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


  
  val reserved = Set('\\', '#', '{', '}')// TODO .: maybe more :. like .: \, #


  
//  def andStar(command:String) =
//  {
//    ("""\s*\*""".r?) ^^ {case Some(_) => command + "*" case None => command}
//  }
//
//
//
  override val whiteSpace = "".r
  
  
//  def syntactic_sugar: Parser[Any] =
  def syntactic_sugar: Parser[Token] =
  {// or preprocessing ??
//    failure("TODO .: not implemented yet")// TODO .: there will be .: comments, double newline, special characters, ... here
    Syntactic_Sugar()
  }.named("syntactic sugar")
  
  
//  def command: Parser[Any] =
  def command: Parser[Token] =
  {
    ( """\\(([@A-Za-z]+)|([^A-Za-z]))""".r >> (Macro(_)()) ) ^^
//    {r => "Command(" + r + ")"}
//    {r => new Compound(List(Simple(r))){override def toString = "Command(" + content.head.asInstanceOf[Simple].content + ")"}}// TODO ...
    {r => r}
  }.named("command")
  
//  def white_space: Parser[Any] =
  def white_space: Parser[Token] =
  {
    """\s+""".r ^^
//    {r => "(" + r + ")"}
    {r => White_Space(r)}// TODO .: what should I do with the white space then ??
  }.named("white_space")

//  def char: Parser[Any] =
  def char: Parser[Token] =
  {
    acceptIf(x => !x.isControl && !reserved(x))(e => "" + e + " is a reserved character") ^^
//    {r => r}
    {r => Character(r)}
  }.named("char")

//  def group: Parser[Any] =
  def group: Parser[Token] =
  {// TODO .: "{"~>rep(token)<~"}" ^^ {r => Group(r)} :. ??
    "{"~>body<~"}" ^^
//    {r => "Group(" + r + ")"}
    {r => r}
  }.named("group")
  
  
//  def lexem: Parser[Any] =
  def token: Parser[Token] =
  {
//    (white_space | char | command | group) ^^
    (syntactic_sugar | white_space | char | command | group) ^^
//    alt(alt(alt(alt(syntactic_sugar, white_space).named("then white_space"), char).named("then char"), command).named("then command"), group).named("then group") ^^
    {r => r}
  }.named("lexem")
  
  
//  def body: Parser[Any] =
  def body: Parser[Token] =
  {
    rep(token) ^^
//    {r => r.mkString(", ")}
    {r => Group(r)}
  }.named("body")
  


}
