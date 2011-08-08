
package subLaTeX



import scala.collection.immutable.Queue
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Reader



object Parser extends Parsers
{
  
  
  
  type Elen = Token
  
  type State = Queue[Elem]// TODO call this context !!!
  
  
  
  /**
   * TODO...
   * parsers passed to this function must combine their results on their own !!!
   *
   */
  def rep_arg[T](arg: T)(pf: T => Parser[T]): Parser[T] =
  {
    pf(arg) >> {a: T => rep_arg(a)(pf)} | success(arg)
  }
  
  
  
  def command(arg: State): Parser[State] =
  {
    elem("command", {case Command(_) => true case _ => false}) ^^
    {r =>
      // TODO .: Macro !!!
      arg + r
    }
  }

  def text(arg: State): Parser[State] =
  {
    rep1(
      elem("character", {case Character(_) => true case _ => false}) |
      elem("white space", {case White_Space(_) => true case _ => false})
    ) ^^
    {r => arg ++ r}
  }
  
  def group(arg: State): Parser[State] = Parser {in =>
    
    in.first match {
      case Group(list) =>
        val state = arg + Command("BEGIN").asInstanceOf[Elem]
        val list_reader = new ListReader(list, ErrorToken("end of list"))
//        body(state)(list_reader.asInstanceOf[Reader[Elem]]) match {// maybe I don't need phrase .?
        phrase(body(state))(list_reader.asInstanceOf[Reader[Elem]]) match {
          case Success(ret, rest) =>
            Success(ret + Command("END").asInstanceOf[Elem], in.rest)
          case ns: NoSuccess => ns
        }
      case _ => Failure("group expected but " + in.first + " found", in)
    }
    
  }
  
  def body(arg: State): Parser[State] =
  {
    rep_arg(arg)(arg =>
      command(arg) | text(arg) | group(arg)
    )
  }
  
  
  
  def parseAll(in: Reader[Token]) =
  {
    phrase(body(Queue.empty[Elem]))(in.asInstanceOf[Reader[Elem]])
  }
  
  
  
}


