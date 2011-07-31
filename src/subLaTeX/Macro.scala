
package subLaTeX

/* TODO .:
 *  class Macro
 *  storage for macros
 *  stack for local def-s
 */

class Macro(val name: String, after: Lexer.Parser[Any], code: Any => Token)
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

  def apply(): Lexer.Parser[Token] =
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
    {_ => Control(command)})// TODO .: DEBUG
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
    case ws: White_Space => Lexer.success(ws)
    case _ => Lexer.failure("Control failure: Ignoring white space.")
  })
  
  val macro_arg = ignore_white_space ~> Lexer.token// TODO !!! this should probably be non-Control token !!! ??
  
  val macro_* = ignore_white_space ~> Lexer.opt(Lexer.elem('*'))
  
  
  
  define(new Macro("""\par""",
    """\s*""".r,
    _ => {
      // TODO .: pop softs into local stack !!! on Syntactic level !!!
//      Control("\\par")
      Control("\n\\par\n")// TODO DEBUG
      // TODO .: push stuff from local stack !!! on Syntactic level !!!
      // TODO .: successive \par-s should be ignored :.
    }
  ))
  
  define(new Macro("""\\""",
    macro_*,
    _ => {
      Control("""\newline""")
    }
  ))
  
  define(new Macro("""\newline""",
    ignore_white_space,
    _ => {
      Control("""\newline""")
    }
  ))
  
  define(new Macro("""\section""",
    macro_* ~ macro_arg,// TODO .: some sensible argument encoding ...
    {
      case Lexer.~(None, arg) =>
        Control("""\section TODO{""" + arg + "}")// TODO ...
      case Lexer.~(Some(_), arg) =>
        Control("""\section* TODO{""" + arg + "}")// TODO ...
    }
  ))
  
  define(new Macro("""\subsection""",
    macro_* ~ macro_arg,// TODO .: some sensible argument encoding ...
    {
      case Lexer.~(None, arg) =>
        Control("""\subsection TODO{""" + arg + "}")// TODO ...
      case Lexer.~(Some(_), arg) =>
        Control("""\subsection* TODO{""" + arg + "}")// TODO ...
    }
  ))
  
  define(new Macro("""\textbackslash""",
    ignore_white_space,
    _ => {
      Character('\\')
    }
  ))
  
  define(new Macro("""\ldots""",
    ignore_white_space,
    _ => {
      Character('…')// TODO ... unicode management ...
    }
  ))
  
  define(new Macro("\\\"",
    macro_arg,// TODO .: some sensible argument encoding ... ??
    arg => {
      
      def fun(token: Token): Token = token match
      {// TODO ... unicode management ...
        case Character('A') => Character('Ä')
        case Character('E') => Character('Ë')
        case Character('I') => Character('Ï')
        case Character('O') => Character('Ö')
        case Character('U') => Character('Ü')
        case Character('Y') => Character('Ÿ')
        case Character('a') => Character('ä')
        case Character('e') => Character('ë')
        case Character('i') => Character('ï')
        case Character('o') => Character('ö')
        case Character('u') => Character('ü')
        case Character('y') => Character('ÿ')
        case Group(Nil) => Character('\"')// TODO ???
//        case Group(head::tail) => fun(head)::tail// TODO .. LaTeX is actually ignoring leading white space ...
        case Group(head::tail) => fun(head)// TODO .. LaTeX is actually ignoring leading white space ...
          // TODO .: the last case should return also the rest of the group !!!
        case token => token// TODO .: error ??? !!!
      }

//        fun(arg)// TODO .: make it know it is getting a token !!!
        fun(arg.asInstanceOf[Token])
    }
  ))
  
  define(new Macro("\\~",
    macro_arg,// TODO .: some sensible argument encoding ... ??
    arg => {

      def fun(token: Token): Token = token match
      {// TODO ... unicode management ...
        case Character('A') => Character('Ã')
        case Character('N') => Character('Ñ')
        case Character('O') => Character('Õ')
        case Character('a') => Character('ã')
        case Character('n') => Character('ñ')
        case Character('o') => Character('õ')
        case Character('I') => Character('Ĩ')
        case Character('i') => Character('ĩ')
        case Character('U') => Character('Ũ')
        case Character('u') => Character('ũ')
        case White_Space(_) => Character('~')// TODO .. LaTeX is actually ignoring leading white space ...
        case Group(Nil) => Character('~')
//        case Group(head::tail) => fun(head)::tail// TODO .. LaTeX is actually ignoring leading white space ...
        case Group(head::tail) => fun(head)// TODO .: the last case should return also the rest of the group !!!
        case token => token// TODO .: error ??? !!!
      }

//        fun(arg)// TODO .: make it know it is getting a token !!!
        fun(arg.asInstanceOf[Token])
    }
  ))
  
  // TODO .: abbreviate this as an escapes !!!
  define(new Macro("""\{""",
    Lexer.success(()),
    _ => {
      Character('{')
    }
  ))
  
  define(new Macro("""\}""",
    Lexer.success(),
    _ => {
      Character('}')
    }
  ))

  define(new Macro("""\#""",
    Lexer.success(),
    _ => {
      Character('#')
    }
  ))

  define(new Macro("""\$""",
    Lexer.success(),
    _ => {
      Character('$')
    }
  ))
  
  define(new Macro("""\%""",
    Lexer.success(),
    _ => {
      Character('%')
    }
  ))
  
  define(new Macro("""\&""",
    Lexer.success(),
    _ => {
      Character('&')
    }
  ))
  
  define(new Macro("""\_""",
    Lexer.success(),
    _ => {
      Character('_')
    }
  ))
  
  
  
}


