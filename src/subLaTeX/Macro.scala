
package subLaTeX

/* TODO .:
 *  class Macro
 *  storage for macros
 *  stack for local def-s
 *
 *  !!! parameters should be parsed just as a text, not interpreted
 *  and then interpreted inside code when appropriate !!!
 */

//class Macro(val name: String, after: Lexer.Parser[Any], code: Any => Token)
//{
//  /* TODO
//   *  name (id, key in the storage)
//   *  after parser
//   *    its output should be an argument for code
//   *  code
//   *    should output a Block that is the final result of the parser command
//   *
//   *  copy(clone) for \let !!!
//   */
//
//  def apply(): Lexer.Parser[Token] =
//  {
//    after ^^ code
//  }
//
//}
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
  
//  def empty(command: String) = new Macro("", arg => success(arg),// TODO !!! sensible default !!!
////    {_ => new Compound(List(Simple(command))){override def toString = "Command(" + content.head.asInstanceOf[Simple].content + ")"}})
//    {arg => arg + Command(command)})// TODO .: DEBUG
//  val empty: Macro = empty("")// TODO .: DEBUG
//  // TODO .: default :. if command matches """\\\s+""".r , it is actually "\\ " .. use let for this
//  // .. or nbsp-s will just go higher as a Control-s and be interpreted there ..
//  // TODO !!! .: make reflexive Control-s explicit !!!
  def empty(command: String) = new Macro("") {
    override def parser(arg: Context): Parser[Context] =
    {
      success(arg + Command(command))// TODO .: DEBUG
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
  
  
  
  // standard argument parsing
//  val ignore_white_space = opt(token >> {
////    case ws: White_Space => success(ws)
//    case r @ List(ws: White_Space) => success(r)
//    case _ => failure("Control failure: Ignoring white space.")
//  })
  val ignore_white_space =
    rep(is(Space)) ~ opt(is(Newline) ~ rep(is(Space)))
  
//  val macro_arg = ignore_white_space ~> token// TODO !!! this should probably be non-Control token !!! ??
//  def macro_arg(arg: Context) = ignore_white_space ~> token ^^^ arg// FIXME !!! differently !!!
  
  val macro_* = ignore_white_space ~> opt(elem('*'))
//  def macro_*(arg: Context) = ignore_white_space ~> opt(elem('*')) ^^^ arg// FIXME !!! differently !!!
  
  
  
//  define(new Macro("par",
//    rep(space | newline),
//    _ => {
//      // TODO .: pop softs into local stack !!! on Syntactic level !!!
////      List(Control("par"))
//      Command("\npar\n")// TODO DEBUG
//      // TODO .: push stuff from local stack !!! on Syntactic level !!!
//      // TODO .: successive \par-s should be ignored :.
//    }
//  ))
  define(new Macro("par") {
    override def parser(arg: Context): Parser[Context] =
    {
      rep(is(Space) | is(Newline)) ^^^
      {arg + Command("\npar\n")}
    }
  })
  
//  define(new Macro("""\""",
//    macro_*,
//    arg => {
//      arg + Command("""newline""")
//    }
//  ))
  define(new Macro("""\""") {
    override def parser(arg: Context): Parser[Context] =
    {
      macro_* <~ ignore_white_space ^^^
      {arg + Command("newline")}
    }
  })
  
//  define(new Macro("newline",
//    ignore_white_space,
//    arg => {
//      Command("newlineDEBUG")// TODO DEBUG
//    }
//  ))
  define(new Macro("newline") {
    override def parser(arg: Context): Parser[Context] =
    {
      ignore_white_space ^^^
      {arg + Command("newline")}
    }
  })
  
//  define(new Macro("section",// FIXME !!! does not work properly
//    macro_* ~ macro_arg,// TODO .: some sensible argument encoding ...
//    {
//      case None~arg =>
//        Command("""section TODO{""" + arg + "}")// TODO ...
//      case Some(_)~arg =>
//        Command("""section* TODO{""" + arg + "}")// TODO ...
//    }
//  ))
  define(new Macro("section") {
    override def parser(arg: Context): Parser[Context] =
    {
      /* TODO .:
       *  first check macro_*
       *  set context accordingly
       *  parse argument
       *  unset context accordingly
       *  and return it
       */
      
      macro_* <~ ignore_white_space >> {
        case Some(_) =>
          val context = arg + Command("section*BEGIN")// TODO ...
          ( command(context) | character(context) | group(context) ) ^^
          {r => r + Command("section*END")}// TODO ...
        case None =>
          val context = arg + Command("sectionBEGIN")// TODO ...
          ( command(context) | character(context) | group(context) ) ^^
          {r => r + Command("sectionEND")}// TODO ...
      }
    }
  })
  
//  define(new Macro("subsection",
//    macro_* ~ macro_arg,// TODO .: some sensible argument encoding ...
//    {
//      case None~arg =>
//        Command("""subsection TODO{""" + arg + "}")// TODO ...
//      case Some(_)~arg =>
//        Command("""subsection* TODO{""" + arg + "}")// TODO ...
//    }
//  ))
  define(new Macro("subsection") {
    override def parser(arg: Context): Parser[Context] =
    {
      /* TODO .:
       *  first check macro_*
       *  set context accordingly
       *  parse argument
       *  unset context accordingly
       *  and return it
       */

      macro_* <~ ignore_white_space >> {
        case Some(_) =>
          val context = arg + Command("subsection*BEGIN")// TODO ...
          ( command(context) | character(context) | group(context) ) ^^
          {r => r + Command("subsection*END")}// TODO ...
        case None =>
          val context = arg + Command("subsectionBEGIN")// TODO ...
          ( command(context) | character(context) | group(context) ) ^^
          {r => r + Command("subsectionEND")}// TODO ...
      }
    }
  })
  
  def char_macro(name: String, char: Char) = new Macro(name) {
    override def parser(arg: Context): Parser[Context] =
    {
      ignore_white_space ^^^
      {arg + Character(char)}
    }
  }
  
  define(char_macro("{", '{'))
  define(char_macro("}", '}'))
  define(char_macro("#", '#'))
  define(char_macro("$", '$'))
  define(char_macro("%", '%'))
  define(char_macro("&", '&'))
  define(char_macro("_", '_'))
  
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


