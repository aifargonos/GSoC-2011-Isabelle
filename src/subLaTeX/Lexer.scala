
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


  
  def andStar(command:String) =
  {
    ("""\s*\*""".r?) ^^ {case Some(_) => command + "*" case None => command}
  }
  
  
/*
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
  */
  
  
  override val whiteSpace = "".r
  
  
//  def syntactic_sugar: Parser[Any] =
  def syntactic_sugar: Parser[Block] =
  {// or preprocessing ??
//    failure("TODO .: not implemented yet")// TODO .: there will be .: comments, double newline, special characters, ... here
    Syntactic_Sugar()
  }.named("syntactic sugar")
  
  
//  def command: Parser[Any] =
  def command: Parser[Block] =
  {
    ( ("""\\(([@A-Za-z]+)|(\\))""".r >> andStar | """\\[^A-Za-z]""".r) >> (Macro(_)()) ) ^^
//    {r => "Command(" + r + ")"}
//    {r => new Compound(List(Simple(r))){override def toString = "Command(" + content.head.asInstanceOf[Simple].content + ")"}}// TODO ...
    {r => r}
  }.named("command")
  
//  def white_space: Parser[Any] =
  def white_space: Parser[Block] =
  {
    """\s+""".r ^^
//    {r => "(" + r + ")"}
    {r => Simple(r)}// TODO .: what should I do with the white space then ??
  }.named("white_space")

//  def char: Parser[Any] =
  def char: Parser[Block] =
  {
    acceptIf(x => !x.isControl && !reserved(x))(e => "" + e + " is a reserved character") ^^
//    {r => r}
    {r => Simple(r.toString)}
  }.named("char")

//  def group: Parser[Any] =
  def group: Parser[Block] =
  {
    "{"~>body<~"}" ^^
//    {r => "Group(" + r + ")"}
    {r => r}
  }.named("group")
  
  
//  def lexem: Parser[Any] =
  def lexem: Parser[Block] =
  {
//    (white_space | char | command | group) ^^
    (syntactic_sugar | white_space | char | command | group) ^^
//    alt(alt(alt(alt(syntactic_sugar, white_space).named("then white_space"), char).named("then char"), command).named("then command"), group).named("then group") ^^
    {r => r}
  }.named("lexem")
  
  
//  def body: Parser[Any] =
  def body: Parser[Block] =
  {
    rep(lexem) ^^
//    {r => r.mkString(", ")}
    {// combines successive Simple-s :. TODO .: this should be done in Block ...
      case Nil => Compound(Nil)
      case head::tail =>
        val buff = new ListBuffer[Block]
        val last = (head /: tail) {(block1, block2) =>
          block1 match {
            case b1: Simple =>
              block2 match {
                case b2: Simple => b1 + b2
                case b2: Compound =>
                  buff += b1
                  b2
              }
            case b1: Compound =>
              buff += b1
              block2
          }
        }
        buff += last
        Compound(buff.toList)
    }
  }.named("body")
  


}
