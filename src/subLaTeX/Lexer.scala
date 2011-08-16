
package subLaTeX



import scala.collection.immutable.Queue
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.input.Reader


/* DONE
 *  whiteSpace !!!
 *  group is either one char or {body}
 *  syntacticSugar
 *  results = tokens
 */

object Lexer extends Parsers
{
  
  
  
  abstract sealed class Push
  // for environments
  case class NamedPush() extends Push// TODO ...
  // for explicit groups
  case class HardPush() extends Push
  // for implicit groups
  case class SoftPush(var begin: Token, var end: Token) extends Push
  
  
  
  type Elem = Char
  
  
  
  def char_err(expected: String)(e: Elem) = expected + " expected but " + e + " found"
  
  // TODO .. do propper character ignoration (Ignore)
  
  def is(cc: CatCode): Parser[Elem] = acceptIf(cc(_))(char_err(cc.toString))
  
  def is_not(cc: CatCode): Parser[Elem] =
    acceptIf(c => !cc(c) && !Ignore(c) && !Invalid(c))(char_err("non " + cc.toString))
  
  
  
//  def command: Parser[Token] =
//  {
//    is(Escape) ~> (
//      rep1(is(Letter) | elem('@')) ^^ {r => r.mkString} |
//      is_not(Letter) ^^ {_.toString}
//    ) ^^
//    {r => Command(r)}
//  }.named("command")
//
//  def white_space: Parser[Token] =
//  {
//    rep1(is(Space) | is(Newline)) ^^
//    {r => White_Space(r.mkString)}
//  }.named("white_space")
//  
//  def character: Parser[Token] =
//  {
//    (is(Letter) | is(Other)) ^^
//    {r => Character(r)}
//  }.named("char")
  
  
  
  /* \section{syntactic level} */
  
  type Context = Tuple2[List[Push], Queue[Token]]
  
  
  
  /**
   * TODO...
   * parsers passed to this function must combine their results on their own !!!
   *
   */
  def rep_arg[T](arg: T)(pf: T => Parser[T]): Parser[T] =
  {
    pf(arg) >> {a: T => rep_arg(a)(pf)} | success(arg)
  }


  
  def syntactic_sugar(arg: Context): Parser[Context] =
  {// or preprocessing ??
    Syntactic_Sugar(arg)
  }//TODO .: .named("syntactic sugar")
  
  def command(arg: Context): Parser[Context] =
  {
    is(Escape) ~> (
      rep1(is(Letter) | elem('@')) ^^ {r => r.mkString} |
      is_not(Letter) ^^ {_.toString}
    ) >>
    {Macro(_)(arg)}
  }.named("command")
  
  def character(arg: Context): Parser[Context] =
  {
    (is(Letter) | is(Other)) ^^
    {r => (arg._1, arg._2 enqueue Character(r))}
  }.named("char")
  
  def white_space(arg: Context): Parser[Context] =
  {
    rep1(is(Space) | is(Newline)) ^^
    {r => (arg._1, arg._2 enqueue White_Space(r.mkString))}
  }.named("white_space")

  def begin(arg: Context): Parser[Context] =
    is(Begin) ^^^ {
//      (HardPush() :: arg._1, arg._2 enqueue Command("HardPush"))// TODO .: command
      (HardPush() :: arg._1, arg._2)
    }
  def end(arg: Context): Parser[Context] =
    is(End) ^^^ {
      
      def loop(arg: Context): Context =
      {
        arg._1 match {
          case Nil =>
            error("HardPush expected, but non found !!!")// TODO ...
          case NamedPush()::stack =>
            error("unexpected NamedPush !!!")// TODO ...
          case HardPush()::stack =>
//            (stack, arg._2 enqueue Command("HardPop"))// TODO .: command
            (stack, arg._2)
          case SoftPush(_, end)::stack =>
            loop( (stack, arg._2 enqueue end) )
        }
      }
      
//      (arg._1, arg._2 enqueue Command("END"))
      loop(arg)
    }
  def group(arg: Context): Parser[Context] =
  {
    begin(arg) >> body >> end
  }.named("group")
  
  def body(arg: Context): Parser[Context] =
  {
    rep_arg(arg){arg =>
      syntactic_sugar(arg) | command(arg) | character(arg) | white_space(arg) | group(arg)
    }
  }
  
  
  // TODO .: all these functions :.
  def parseAll(in: Reader[Char]): Seq[Token] =
  {
    phrase(
      body( (Nil, Queue.empty[Token] enqueue Par_Begin()) )
    )(in.asInstanceOf[Reader[Elem]]) match {// just to add parEND to the end ...
      case Success( arg, next ) =>
        
        def loop(arg: Context): Context =
        {
          arg._1 match {
            case Nil =>
              arg
            case NamedPush()::stack =>
              error("unexpected NamedPush !!!")// TODO ...
            case HardPush()::stack =>
              error("unexpected HardPush !!!")// TODO ...
            case SoftPush(_, end)::stack =>
              loop( (stack, arg._2 enqueue end) )
          }
        }
        val (stack, out) = loop(arg)
        
        if(!stack.isEmpty) error("Stack is not empty at the end of parsing !!!\nIt contains: " + stack) else
        out enqueue Par_End()
      case ns: NoSuccess => error(ns.toString)
    }
  }
  
  def parseAll(in: CharSequence): Seq[Token] =
  {
    parseAll(new CharSequenceReader(in))
  }

  
  
}
