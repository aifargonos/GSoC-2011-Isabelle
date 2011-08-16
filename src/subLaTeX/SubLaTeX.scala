
package subLaTeX



object SubLaTeX
{
  
  
  
  private def command2string(command: Command) =
  command match {
    case Unknown(c) => """\""" + c// TODO yell more .. some warning or stuff ..
    case Par_Begin() => "<p>\n"
    case Par_End() => "\n</p>"
    case Line_Break() => "<br/>\n"
    case NBSP() => "&nbsp;"
    case Section_Begin() => "<h2>"// TODO styling ??
    case Section_End() => "</h2>"// TODO styling ??
    case Subsection_Begin() => "<h3>"// TODO styling ??
    case Subsection_End() => "</h3>"// TODO styling ??
    case Subsubsection_Begin() => "<h4>"// TODO styling ??
    case Subsubsection_End() => "</h4>"// TODO styling ??
    case Bf_Begin() => """<span style="font-weight:bold;">"""// TODO separate css ??
    case Bf_End() => "<!--end bold--></span>"
    case It_Begin() => """<span style="font-style:italic;">"""// TODO separate css ??
    case It_End() => "<!--end italic--></span>"
    case Large_Begin() => """<span style="font-size:x-large;">"""// TODO separate css ??
    case Large_End() => "<!--end size--></span>"
  }
  
  def output(tokens: Seq[Token]): CharSequence =
  {
    val ret = for(token <- tokens) yield {// TODO use buffer
      token match {
        case White_Space(s) => s
        case Character(c) => c.toString
        case c: Command => command2string(c)
      }
    }
    ret.mkString
  }
  
  
  
  def apply(in: CharSequence): CharSequence =
  {
    output(Lexer.parseAll(in))
  }
  
  
  
}


