
package subLaTeX

/* TODO .:
 *  class Macro
 *  storage for macros
 *  stack for local def-s
 */

class Macro(val name: String, after: Lexer.Parser[Any], code: Any => List[Token])
{
  /* TODO
   *  name (id, key in the storage)
   *  after parser
   *    its output should be an argument for code
   *  code
   *    should output a Block that is the final result of the parser command
   *
   *  copy(clone) for \let !!!
   */

  def apply(): Lexer.Parser[List[Token]] =
  {
    after ^^ code
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
  
  def empty(command: String) = new Macro("", Lexer.success(()),// TODO !!! sensible default !!!
//    {_ => new Compound(List(Simple(command))){override def toString = "Command(" + content.head.asInstanceOf[Simple].content + ")"}})
    {_ => List(Control(command))})// TODO .: DEBUG
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
  val ignore_white_space = Lexer.opt(Lexer.token >> {
//    case ws: White_Space => Lexer.success(ws)
    case r @ List(ws: White_Space) => Lexer.success(r)
    case _ => Lexer.failure("Control failure: Ignoring white space.")
  })
  
  val macro_arg = ignore_white_space ~> Lexer.token// TODO !!! this should probably be non-Control token !!! ??
  
  val macro_* = ignore_white_space ~> Lexer.opt(Lexer.elem('*'))
  
  
  
  define(new Macro("""\par""",
    """\s*""".r,
    _ => {
      // TODO .: pop softs into local stack !!! on Syntactic level !!!
//      List(Control("\\par"))
      List(Control("\n\\par\n"))// TODO DEBUG
      // TODO .: push stuff from local stack !!! on Syntactic level !!!
      // TODO .: successive \par-s should be ignored :.
    }
  ))
  
  define(new Macro("""\\""",
    macro_*,
    arg => {
      List(Control("""\newline"""))
    }
  ))
  
  define(new Macro("""\newline""",
    ignore_white_space,
    arg => {
      List(Control("""\newlineDEBUG"""))
    }
  ))
  
  define(new Macro("""\section""",// FIXME !!! does not work properly
    macro_* ~ macro_arg,// TODO .: some sensible argument encoding ...
    {
      case Lexer.~(None, arg) =>
        List(Control("""\section TODO{""" + arg + "}"))// TODO ...
      case Lexer.~(Some(_), arg) =>
        List(Control("""\section* TODO{""" + arg + "}"))// TODO ...
    }
  ))
  
  define(new Macro("""\subsection""",
    macro_* ~ macro_arg,// TODO .: some sensible argument encoding ...
    {
      case Lexer.~(None, arg) =>
        List(Control("""\subsection TODO{""" + arg + "}"))// TODO ...
      case Lexer.~(Some(_), arg) =>
        List(Control("""\subsection* TODO{""" + arg + "}"))// TODO ...
    }
  ))
  
  define(new Macro("""\textbackslash""",
    ignore_white_space,
    _ => {
      List(Character('\\'))
    }
  ))
  
  define(new Macro("""\ldots""",
    ignore_white_space,
    _ => {
      List(Character('…'))// TODO ... unicode management ...
    }
  ))
  
  
  private def drop_white_space(toks: List[Token]): List[Token] = toks match {
    case Nil => Nil
    case (head: White_Space)::tail =>
      drop_white_space(tail)
    case list => list
  }
  
  define(new Macro("\\'",
    macro_arg,// TODO .: some sensible argument encoding ... ??
    arg => {

      def fun(token: Token): List[Token] = token match
      {// TODO ... unicode management ...
        case Character('A') => List(Character('Á'))
        case Character('E') => List(Character('É'))
        case Character('I') => List(Character('Í'))
        case Character('O') => List(Character('Ó'))
        case Character('U') => List(Character('Ú'))
        case Character('Y') => List(Character('Ý'))
        case Character('a') => List(Character('á'))
        case Character('e') => List(Character('é'))
        case Character('i') => List(Character('í'))
        case Character('o') => List(Character('ó'))
        case Character('u') => List(Character('ú'))
        case Character('y') => List(Character('ý'))
        case Character('C') => List(Character('Ć'))
        case Character('c') => List(Character('ć'))
        case Character('L') => List(Character('Ĺ'))
        case Character('l') => List(Character('ĺ'))
        case Character('N') => List(Character('Ń'))
        case Character('n') => List(Character('ń'))
        case Character('R') => List(Character('Ŕ'))
        case Character('r') => List(Character('ŕ'))
        case Character('S') => List(Character('Ś'))
        case Character('s') => List(Character('ś'))
        case Character('Z') => List(Character('Ź'))
        case Character('z') => List(Character('ź'))
        case Character('G') => List(Character('Ǵ'))
        case Character('g') => List(Character('ǵ'))
        case Group(Nil) => List(Character('´'))
        case Group(toks) =>
          drop_white_space(toks) match {
            case Nil => List(Character('´'))
            case head::tail => List(Group(fun(head):::tail))
          }
        case token => List(token)// TODO .: error ??? !!!
      }

//        fun(arg)// TODO .: make it know it is getting a token !!! argments class
        fun(arg.asInstanceOf[List[Token]].head)
    }
  ))
  
  define(new Macro("\\\"",
    macro_arg,// TODO .: some sensible argument encoding ... ??
    arg => {
      
      def fun(token: Token): List[Token] = token match
      {// TODO ... unicode management ...
        case Character('A') => List(Character('Ä'))
        case Character('E') => List(Character('Ë'))
        case Character('I') => List(Character('Ï'))
        case Character('O') => List(Character('Ö'))
        case Character('U') => List(Character('Ü'))
        case Character('Y') => List(Character('Ÿ'))
        case Character('a') => List(Character('ä'))
        case Character('e') => List(Character('ë'))
        case Character('i') => List(Character('ï'))
        case Character('o') => List(Character('ö'))
        case Character('u') => List(Character('ü'))
        case Character('y') => List(Character('ÿ'))
        case Group(Nil) => List(Character('¨'))
        case Group(toks) =>
          drop_white_space(toks) match {
            case Nil => List(Character('¨'))
            case head::tail => List(Group(fun(head):::tail))
          }
        case token => List(token)// TODO .: error ??? !!!
      }

//        fun(arg)// TODO .: make it know it is getting a token !!! argments class
        fun(arg.asInstanceOf[List[Token]].head)
    }
  ))
  
  define(new Macro("\\^",
    macro_arg,// TODO .: some sensible argument encoding ... ??
    arg => {

      def fun(token: Token): List[Token] = token match
      {// TODO ... unicode management ...
        case Character('A') => List(Character('Â'))
        case Character('E') => List(Character('Ê'))
        case Character('I') => List(Character('Î'))
        case Character('O') => List(Character('Ô'))
        case Character('U') => List(Character('Û'))
        case Character('a') => List(Character('â'))
        case Character('e') => List(Character('ê'))
        case Character('i') => List(Character('î'))
        case Character('o') => List(Character('ô'))
        case Character('u') => List(Character('û'))
        case Character('C') => List(Character('Ĉ'))
        case Character('c') => List(Character('ĉ'))
        case Character('G') => List(Character('Ĝ'))
        case Character('g') => List(Character('ĝ'))
        case Character('H') => List(Character('Ĥ'))
        case Character('h') => List(Character('ĥ'))
        case Character('J') => List(Character('Ĵ'))
        case Character('j') => List(Character('ĵ'))
        case Character('S') => List(Character('Ŝ'))
        case Character('s') => List(Character('ŝ'))
        case Character('W') => List(Character('Ŵ'))
        case Character('w') => List(Character('ŵ'))
        case Character('Y') => List(Character('Ŷ'))
        case Character('y') => List(Character('ŷ'))
        case Group(Nil) => List(Character('^'))
        case Group(toks) =>
          drop_white_space(toks) match {
            case Nil => List(Character('^'))
            case head::tail => List(Group(fun(head):::tail))
          }
        case token => List(token)// TODO .: error ??? !!!
      }

//        fun(arg)// TODO .: make it know it is getting a token !!! argments class
        fun(arg.asInstanceOf[List[Token]].head)
    }
  ))

  define(new Macro("\\~",
    macro_arg,// TODO .: some sensible argument encoding ... ??
    arg => {

      def fun(token: Token): List[Token] = token match
      {// TODO ... unicode management ...
        case Character('A') => List(Character('Ã'))
        case Character('N') => List(Character('Ñ'))
        case Character('O') => List(Character('Õ'))
        case Character('a') => List(Character('ã'))
        case Character('n') => List(Character('ñ'))
        case Character('o') => List(Character('õ'))
        case Character('I') => List(Character('Ĩ'))
        case Character('i') => List(Character('ĩ'))
        case Character('U') => List(Character('Ũ'))
        case Character('u') => List(Character('ũ'))
        case Group(Nil) => List(Character('~'))
        case Group(toks) =>
          drop_white_space(toks) match {
            case Nil => List(Character('~'))
            case head::tail => List(Group(fun(head):::tail))
          }
        case token => List(token)// TODO .: error ??? !!!
      }

//        fun(arg)// TODO .: make it know it is getting a token !!! argments class
        fun(arg.asInstanceOf[List[Token]].head)
    }
  ))
  
  // TODO .: abbreviate this as an escapes !!!
  define(new Macro("""\{""",
    Lexer.success(()),
    _ => {
      List(Character('{'))
    }
  ))
  
  define(new Macro("""\}""",
    Lexer.success(),
    _ => {
      List(Character('}'))
    }
  ))

  define(new Macro("""\#""",
    Lexer.success(),
    _ => {
      List(Character('#'))
    }
  ))

  define(new Macro("""\$""",
    Lexer.success(),
    _ => {
      List(Character('$'))
    }
  ))
  
  define(new Macro("""\%""",
    Lexer.success(),
    _ => {
      List(Character('%'))
    }
  ))
  
  define(new Macro("""\&""",
    Lexer.success(),
    _ => {
      List(Character('&'))
    }
  ))
  
  define(new Macro("""\_""",
    Lexer.success(),
    _ => {
      List(Character('_'))
    }
  ))
  
  
  
}


