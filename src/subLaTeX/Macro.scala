
package subLaTeX

/* TODO .:
 *  class Macro
 *  storage for macros
 *  stack for local def-s
 *
 *  !!! parameters should be parsed just as a text, not interpreted
 *  and then interpreted inside code when appropriate !!!
 */

import scala.collection.immutable.Queue

abstract class Macro(val name: String)
{

  def parser(arg: Lexer.Context): Lexer.Parser[Lexer.Context]

  def apply(arg: Lexer.Context): Lexer.Parser[Lexer.Context] =
  {
    parser(arg)
  }
  
}



object Macro
{
  /* TODO
   *  storage for macros
   *    defining
   *    removing ??
   *  define basic macros !!!
   */
  
  import Lexer._
  
  def empty(command: String) = new Macro("") {
    override def parser(arg: Context): Parser[Context] =
    {
      success((arg._1, arg._2 enqueue Unknown(command)))// TODO .: DEBUG
    }
  }
  val empty: Macro = empty("")// TODO .: DEBUG
  // TODO .: default :. if command matches """\\\s+""".r , it is actually "\\ " .. use let for this
  // .. or nbsp-s will just go higher as a Control-s and be interpreted there ..
  // TODO !!! .: make reflexive Control-s explicit !!!
  
  val storage = scala.collection.mutable.Map.empty[String, Macro]

  def apply(name: String): Macro =
  {
    storage.getOrElse(name, empty(name))
  }
  
  def define(macro: Macro) =
  {// TODO .: check redefinitions !!!
    storage.put(macro.name, macro)
  }
  
  def alias(alias: String, for_macro: String) = define(new Macro(alias) {// TODO ??
    override def parser(arg: Context): Parser[Context] = Macro(for_macro)(arg)
  })
  
  def let(new_name: String, old_name: String) =
  {// TODO ?? check how closures work !!
    val macro = Macro(old_name)
    define(new Macro(new_name) {
      override def parser(arg: Context): Parser[Context] = macro(arg)
    })
  }
  
  
  
  // FIXME !!! what if this is an implicit par ?? !!!
  val ignore_white_space =
//    rep(is(Space)) ~ opt( is(Newline) ~ rep(is(Space)) )
    rep(is(Space))
  // TODO .: I should do it this way .:
  def ignore_white_space(arg: Context): Parser[Context] =
    syntactic_sugar(arg) |
    rep(is(Space)) ~ opt( is(Newline) ~ rep(is(Space)) ) ^^^ {arg}
  
//  val macro_arg = ignore_white_space ~> token// TODO !!! this should probably be non-Control token !!! ??
//  def macro_arg(arg: Context) = ignore_white_space ~> token ^^^ arg// FIXME !!! differently !!!
  
  val macro_* = ignore_white_space ~> opt(elem('*'))
//  def macro_*(arg: Context) = ignore_white_space ~> opt(elem('*')) ^^^ arg// FIXME !!! differently !!!
  
  

  /**
   * pop stack until NamedPush, output SoftPush's end-s, push everything popped into local
   */
  private def pop(stack: List[Push], out: Queue[Token], local: List[Push]):
    (List[Push], Queue[Token], List[Push]) =
  {
    stack match {
      case Nil =>
        (stack, out, local)
      case NamedPush()::rest =>
        (stack, out, local)
      case (p @ HardPush())::rest =>
//      pop(rest, out enqueue Command("HardPop"), p::local)
        pop(rest, out, p::local)
      case (p @ SoftPush(_, end))::rest =>
        pop(rest, out enqueue end, p::local)
    }
  }
  
  /**
   * push everything from local back to stack, output SoftPush's begins
   */
  private def push(stack: List[Push], out: Queue[Token], local: List[Push]):
    (List[Push], Queue[Token], List[Push]) =
  {
    local match {
      case Nil =>
        (stack, out, local)
      case NamedPush()::rest =>
        error("unexpected NamedPush")// TODO ..
      case (p @ HardPush())::rest =>
//      push(p::stack, out enqueue Command("HardPush"), rest)
        push(p::stack, out, rest)
      case (p @ SoftPush(begin, _))::rest =>
        push(p::stack, out enqueue begin, rest)
    }
  }
  
  define(new Macro("par") {
    override def parser(arg: Context): Parser[Context] =
    {
      rep(is(Space) | is(Newline)) ^^^
      {
        /* DONE .:
         *  pop until NamedPush or everything, output all SoftPush-es and stack it to a local stack
         *  new par
         *  push everything form the local stack and output all SoftPush-es
         * DONE .:
         *  output soft end / soft start
         */
        
        val (stack, out, local) = pop(arg._1, arg._2, Nil)
        val out_with_par = out enqueue White_Space("\n") enqueue Par_End() enqueue Par_Begin() enqueue White_Space("\n")
        val (s, o, l) = push(stack, out_with_par, local)
        
        (s, o)
//        (arg._1, o)// TODO DEBUG
//        (arg._1, arg._2 enqueue Command("\npar\n"))
      }
    }
  })
  
  define(new Macro("newline") {
    override def parser(arg: Context): Parser[Context] =
    {
      ignore_white_space ^^^
      {(arg._1, arg._2 enqueue Line_Break())}
    }
  })
  define(new Macro("""\""") {
    override def parser(arg: Context): Parser[Context] =
    {
      macro_* ^^^
      {(arg._1, arg._2 enqueue Line_Break())}
    }
  })
  
  define(new Macro(" ") {
    override def parser(arg: Context): Parser[Context] =
    {
      ignore_white_space ^^^
      {(arg._1, arg._2 enqueue NBSP())}
    }
  })
  alias("\n", " ")
  alias("\t", " ")
  
  
  define(new Macro("bfseries") {
    override def parser(arg: Context): Parser[Context] =
    {
      ignore_white_space ^^^
      {
        (
          SoftPush(Bf_Begin(), Bf_End())::arg._1,
          arg._2 enqueue Bf_Begin()
        )
     }
    }
  })
  
  define(new Macro("itshape") {
    override def parser(arg: Context): Parser[Context] =
    {
      ignore_white_space ^^^
      {
        (
          SoftPush(It_Begin(), It_End())::arg._1,
          arg._2 enqueue It_Begin()
        )
     }
    }
  })
  
  define(new Macro("Large") {
    override def parser(arg: Context): Parser[Context] =
    {
      ignore_white_space ^^^
      {
        (
          SoftPush(Large_Begin(), Large_End())::arg._1,
          arg._2 enqueue Large_Begin()
        )
     }
    }
  })

  define(new Macro("normalfont") {// TODO .: this also needs some stacking .. to work inside a group
    override def parser(arg: Context): Parser[Context] =
    {
      ignore_white_space ^^^
      {
        /*
         * pop all font-formating SoftPush-es
         */
        
        def loop(arg: Context): Context =
        {
          arg._1 match {
            case Nil =>
              arg
            case NamedPush()::stack =>
              arg
            case HardPush()::stack =>
              arg
            case SoftPush(_, end)::stack =>
              loop( (stack, arg._2 enqueue end) )// TODO .: only font-formating SoftPush-es !!!
          }
        }
        
        loop(arg)
//        (SoftPush(Command("bfseries"))::arg._1, arg._2 enqueue Command("bfseries"))
      }
    }
  })
  // TODO .: \normalsize
  
  define(new Macro("textbf") {// TODO .: define this as an user macro !!!
    override def parser(arg: Context): Parser[Context] =
    {
      /* This does .:
       *  HardPush
       *  call bfseries
       *  parse arg
       *  finalise SoftPush-es and pop HardPush ..
       */
      
      val context = (HardPush()::arg._1, arg._2)

      Macro("bfseries")(context) >> (arg =>
        command(arg) |
        character(arg) |
        is(Begin) ~> body(arg) <~ is(End)
      ) ^^ {r =>

        def loop(arg: Context): Context =
        {
          arg._1 match {
            case Nil =>
              error("HardPush expected, but non found !!!")// TODO ...
            case NamedPush()::stack =>
              error("unexpected Push !!!")// TODO ...
            case HardPush()::stack =>
              (stack, arg._2)
            case SoftPush(_, end)::stack =>
              loop( (stack, arg._2 enqueue end) )
          }
        }

        loop(r)
      }
    }
  })
  
  
  def section_macro(level: String) = new Macro(level) {// TODO .: other arguments, like level !!!
    override def parser(arg: Context): Parser[Context] =
    {
      /* This way .:
       *  finalise the whole stack (and restore it after the execution .. so I need to save it ...)
       *  BEGIN command (tag/style)
       *  according to macro_* do the counters and numbering
       *  parse the macro argument with an empty local stack
       *  finalise the local stack
       *  END command (tag/style)
       *  restore the original stack ...
       */

      // finalise the whole stack (and restore it after the execution .. so I need to save it ...)
      val (stack, out, popped) = pop(arg._1, arg._2, Nil)// TODO what about NamedPush-es ??

      // BEGIN command (tag/style)
      val out_with_begin = out enqueue Par_End() enqueue (level match {
        case "section" => Section_Begin()
        case "subsection" => Subsection_Begin()
        case "subsubsection" => Subsubsection_Begin()
      })// TODO .: these cases are magic constants !!

      macro_* <~ ignore_white_space >> {the_* =>

        // according to macro_* do the counters and numbering
        val context = (Nil, the_* match {
          case Some(_) => out_with_begin
          case None => out_with_begin enqueue Unknown(name + "NUMBER")// TODO commands ...
        })

        // parse the macro argument with an empty local stack
        ( command(context) | character(context) | group(context) ) ^^
        {r =>
          // finalise the local stack
          def loop(arg: Context): Context =
          {
            arg._1 match {
              case Nil =>
                arg
              case NamedPush()::stack =>
                error("Unexpected NamedPush")// TODO ...
              case HardPush()::stack =>
                error("Unexpected HardPush")// TODO ...
              case SoftPush(_, end)::stack =>
                loop( (stack, arg._2 enqueue end) )
            }
          }
          val (local, out) = loop(r)
          require(local.isEmpty)// TODO ??

          // END command (tag/style)
          val out_with_end = out enqueue (level match {// TODO .: these cases are magic constants !!
            case "section" => Section_End()
            case "subsection" => Subsection_End()
            case "subsubsection" => Subsubsection_End()
          }) enqueue Par_Begin()

          // restore the original stack ...
          val (s, o, l) = push(stack, out_with_end, popped)

          (s, o)
        }

      }
    }
  }
  define(section_macro("section"))
  define(section_macro("subsection"))
  define(section_macro("subsubsection"))
  
  
  def escape_macro(char: Char) = new Macro(char.toString) {
    override def parser(arg: Context): Parser[Context] =
    {
      success{(arg._1, arg._2 enqueue Character(char))}
    }
  }
  define(escape_macro('{'))
  define(escape_macro('}'))
  define(escape_macro('#'))
  define(escape_macro('$'))
  define(escape_macro('%'))
  define(escape_macro('&'))
  define(escape_macro('_'))

  def char_macro(name: String, char: Char) = new Macro(name) {
    override def parser(arg: Context): Parser[Context] =
    {
      ignore_white_space ^^^
      {(arg._1, arg._2 enqueue Character(char))}
    }
  }
  define(char_macro("textbackslash", '\\'))
  define(char_macro("ldots", '…'))// TODO ... unicode management ...
  
  
//  private def drop_white_space(toks: List[Token]): List[Token] = toks match {
//    case Nil => Nil
//    case (head: White_Space)::tail =>
//      drop_white_space(tail)
//    case list => list
//  }
//  
//  define(new Macro("\'",
//    macro_arg,// TODO .: some sensible argument encoding ... ??
//    arg => {
//
//      def fun(token: Token): Token = token match
//      {// TODO ... unicode management ...
//        case Character('A') => Character('Á')
//        case Character('E') => Character('É')
//        case Character('I') => Character('Í')
//        case Character('O') => Character('Ó')
//        case Character('U') => Character('Ú')
//        case Character('Y') => Character('Ý')
//        case Character('a') => Character('á')
//        case Character('e') => Character('é')
//        case Character('i') => Character('í')
//        case Character('o') => Character('ó')
//        case Character('u') => Character('ú')
//        case Character('y') => Character('ý')
//        case Character('C') => Character('Ć')
//        case Character('c') => Character('ć')
//        case Character('L') => Character('Ĺ')
//        case Character('l') => Character('ĺ')
//        case Character('N') => Character('Ń')
//        case Character('n') => Character('ń')
//        case Character('R') => Character('Ŕ')
//        case Character('r') => Character('ŕ')
//        case Character('S') => Character('Ś')
//        case Character('s') => Character('ś')
//        case Character('Z') => Character('Ź')
//        case Character('z') => Character('ź')
//        case Character('G') => Character('Ǵ')
//        case Character('g') => Character('ǵ')
//        case Group(Nil) => Character('´')
//        case Group(toks) =>
//          drop_white_space(toks) match {
//            case Nil => Character('´')
//            case head::tail => Group(fun(head)::tail)
//          }
//        case token => token// TODO .: error ??? !!!
//      }
//
////        fun(arg)// TODO .: make it know it is getting a token !!! argments class
//        fun(arg.asInstanceOf[Token])
//    }
//  ))
//
//  define(new Macro("\"",
//    macro_arg,// TODO .: some sensible argument encoding ... ??
//    arg => {
//
//      def fun(token: Token): Token = token match
//      {// TODO ... unicode management ...
//        case Character('A') => Character('Ä')
//        case Character('E') => Character('Ë')
//        case Character('I') => Character('Ï')
//        case Character('O') => Character('Ö')
//        case Character('U') => Character('Ü')
//        case Character('Y') => Character('Ÿ')
//        case Character('a') => Character('ä')
//        case Character('e') => Character('ë')
//        case Character('i') => Character('ï')
//        case Character('o') => Character('ö')
//        case Character('u') => Character('ü')
//        case Character('y') => Character('ÿ')
//        case Group(Nil) => Character('¨')
//        case Group(toks) =>
//          drop_white_space(toks) match {
//            case Nil => Character('¨')
//            case head::tail => Group(fun(head)::tail)
//          }
//        case token => token// TODO .: error ??? !!!
//      }
//
////        fun(arg)// TODO .: make it know it is getting a token !!! argments class
//        fun(arg.asInstanceOf[Token])
//    }
//  ))
//
//  define(new Macro("^",
//    macro_arg,// TODO .: some sensible argument encoding ... ??
//    arg => {
//
//      def fun(token: Token): Token = token match
//      {// TODO ... unicode management ...
//        case Character('A') => Character('Â')
//        case Character('E') => Character('Ê')
//        case Character('I') => Character('Î')
//        case Character('O') => Character('Ô')
//        case Character('U') => Character('Û')
//        case Character('a') => Character('â')
//        case Character('e') => Character('ê')
//        case Character('i') => Character('î')
//        case Character('o') => Character('ô')
//        case Character('u') => Character('û')
//        case Character('C') => Character('Ĉ')
//        case Character('c') => Character('ĉ')
//        case Character('G') => Character('Ĝ')
//        case Character('g') => Character('ĝ')
//        case Character('H') => Character('Ĥ')
//        case Character('h') => Character('ĥ')
//        case Character('J') => Character('Ĵ')
//        case Character('j') => Character('ĵ')
//        case Character('S') => Character('Ŝ')
//        case Character('s') => Character('ŝ')
//        case Character('W') => Character('Ŵ')
//        case Character('w') => Character('ŵ')
//        case Character('Y') => Character('Ŷ')
//        case Character('y') => Character('ŷ')
//        case Group(Nil) => Character('^')
//        case Group(toks) =>
//          drop_white_space(toks) match {
//            case Nil => Character('^')
//            case head::tail => Group(fun(head)::tail)
//          }
//        case token => token// TODO .: error ??? !!!
//      }
//
////        fun(arg)// TODO .: make it know it is getting a token !!! argments class
//        fun(arg.asInstanceOf[Token])
//    }
//  ))
//
//  define(new Macro("~",
//    macro_arg,// TODO .: some sensible argument encoding ... ??
//    arg => {
//
//      def fun(token: Token): Token = token match
//      {// TODO ... unicode management ...
//        case Character('A') => Character('Ã')
//        case Character('N') => Character('Ñ')
//        case Character('O') => Character('Õ')
//        case Character('a') => Character('ã')
//        case Character('n') => Character('ñ')
//        case Character('o') => Character('õ')
//        case Character('I') => Character('Ĩ')
//        case Character('i') => Character('ĩ')
//        case Character('U') => Character('Ũ')
//        case Character('u') => Character('ũ')
//        case Group(Nil) => Character('~')
//        case Group(toks) =>
//          drop_white_space(toks) match {
//            case Nil => Character('~')
//            case head::tail => Group(fun(head)::tail)
//          }
//        case token => token// TODO .: error ??? !!!
//      }
//
////        fun(arg)// TODO .: make it know it is getting a token !!! argments class
//        fun(arg.asInstanceOf[Token])
//    }
//  ))
  
  
  
}


