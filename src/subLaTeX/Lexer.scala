
package subLaTeX



import scala.util.parsing.combinator.RegexParsers


/* TODO
 *  whiteSpace !!!
 *  group is either one char or {body}
 *  syntacticSugar
 *  results
 */

object Lexer {
  
  def rpt(s: String, i: Int): String = s*i


  val reserved = Set("{", "}")// TODO .: maybe more :. like .: \, #
//  val reserved = Set('{', '}')// TODO .: maybe more :. like .: \, #


}

class Lexer extends RegexParsers {
  

  import Lexer._

  var ind: Int = 0
  
  def alt[T](p: => Parser[T], q: => Parser[T]) = new Parser[T] {
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
          println(rpt("\t",ind) + "q results in:" + (ret match{case Success(r, _) => r case failure => "failure"}) )
          ind -= 1
          ret
      }
    }
  }


  def cond[T](p: => Parser[T], c: T => Boolean) = new Parser[T] {// TODO .: use acceplIf
    def apply(in: Input) = p(in) match {
        case s1 @ Success(r, _) =>
          if(c(r)) s1 else Failure("" + r + " does not satisfy a condition", in)
        case failure => failure
      }
  }.named("cond")
  
  
  
//  class DynamicParser[T]() extends Parser[T]
//  {// TODO .: cannot find Parsers .. don't know why ..
//    // TODO !!!
//    def apply(in: Input) =
//    {
//      // TODO !!!
//      scala.util.parsing.combinator.Parsers.Failute("TODO !!! Not Implemented, yet !!!", in)
//    }
//  }
//
//  def dynamic[T,U](p: => Parser[T])(following: T => Parser[U]): Parser[U] = new Parser[U]// TODO .: use "into" !!!
//  {
//    def apply(in: Input) =
//    {
//      p(in) match {
//        case s1 @ Success(result, next) =>
//          following(result)(next)
//        case failure => failure.asInstanceOf[NoSuccess]
//      }
//    }
//  }
  
  def andStar(command:String) =
  {// TODO .: handleWhitespace explicitly ??
//    opt(elem('*')) ^^ {case Some(r) => command + r case None => command}
////    (elem('*')?) ^^ {case Some(r) => command + r case None => command}
//    opt("""*""") ^^ {case Some(_) => command + "*" case None => command}
    ("""*"""?) ^^ {case Some(_) => command + "*" case None => command}
  }
  
  
  
  def word: Parser[Any] =
  {
    """[A-Za-z]+""".r ^^
    {r => "Word(" + r + ")"}
  }.named("word")
  
  def number: Parser[Any] =
  {
    """[0-9]+""".r ^^
    {r => "Num(" + r + ")"}
  }.named("number")

//  def command: Parser[Any] =
//  {// TODO !!! .: use one regex for this !!! there must be no spaces !!! and """\ """ is also a command !!! """\\*""" becomes Command(\), Char(*) !!!
////    """\"""~>("""[@A-Za-z]+\*?""".r | """[^A-Za-z]""".r | """\*""") ^^
//    """\\(([@A-Za-z]+\*?)|(\\\*)|([^A-Za-z]))""".r ^^
//    {r => "Command(" + r + ")"}// TODO .: * can be actually separated by white spaces
//  }.named("command")
//
  def command: Parser[Any] =
  {
    ("""\\(([@A-Za-z]+)|(\\))""".r >> andStar | """\\[^A-Za-z]""".r) ^^
    {r => "Command(" + r + ")"}
  }.named("command")

  def group: Parser[Any] =
  {
    "{"~>body<~"}" ^^
    {r => "Group(" + r + ")"}
  }.named("group")
  
  def char: Parser[Any] =
  {
    cond(""".""".r, (x:String) => !reserved(x)) ^^
//    acceptIf(!reserved(_))(e => "" + e + " is a reserved character") ^^TODO .: this is looping .. I don't know why ..
    {r => "Char(" + r + ")"}
  }.named("char")

  def text: Parser[Any] =
  {
    (word | number | command | group | char) ^^
//    (word | number | command | group) ^^
//    (word | number | command | char | group) ^^
//    alt(alt(alt(alt(word, number).named("then number"), command).named("then command"), group).named("then group"), char).named("then char") ^^
    {r => r}
  }.named("text")
  
  def body: Parser[Any] =
  {
    rep(text) ^^
    {r => r.mkString(", ")}
  }.named("body")
  

//  def lod: Parser[Any] = "lod"
//  def p: Parser[Any] = ("p"~lod) | "plod"



}
