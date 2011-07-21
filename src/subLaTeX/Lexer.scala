
package subLaTeX



import scala.util.parsing.combinator.RegexParsers


/* TODO
 *  whiteSpace !!!
 *  group is either one char or {body}
 *  syntacticSugar
 *  results
 */

object Lexer {
  
  def rpt(s: String, i: Int): String = s*i// TODO .: debug


//  val reserved = Set("{", "}")// TODO .: maybe more :. like .: \, #
  val reserved = Set('{', '}')// TODO .: maybe more :. like .: \, #


}

class Lexer extends RegexParsers {
  

  import Lexer._

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


  
  def andStar(command:String) =
  {// TODO .: handleWhitespace explicitly ??
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
    """\s*""".r ~>
    acceptIf(x => !x.isControl && !reserved(x))(e => "" + e + " is a reserved character") ^^
    {r => "Char(" + r + ")"}
  }.named("char")

  def text: Parser[Any] =
  {
    (word | number | command | group | char) ^^
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
