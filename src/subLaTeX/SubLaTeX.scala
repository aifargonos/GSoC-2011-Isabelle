
package subLaTeX


/*
 * TODO .:
 *  separate css and styling
 *  postprocessing
 *    ignoring empty paragraphs ...
 */

object SubLaTeX
{
  
  
  
  private def character2string(c: Char) =
  c match {
    case '&' => "&amp;"
    case '<' => "&lt;"
    case '>' => "&gt;"
//    case '\'' => "&apos;"
//    case '"' => "&quot;"
    case c if c < 127 => c.toString// ASCII
    case c => "&#" + c.toInt + ";"// UTF-8
  }

  private def command2string(command: Command) =
  command match {
    case Unknown(c) =>
      Console.err.println("""WARNING: Ignoring unknown command "\""" + c + "\"")
      ""
    case Par_Begin() => "<p>\n"
    case Par_End() => "\n</p>"
    case Line_Break() => "<br/>\n"
    case NBSP() => "&nbsp;"
    case Section_Begin() => "<h2>"
    case Section_End() => "</h2>"
    case Subsection_Begin() => "<h3>"
    case Subsection_End() => "</h3>"
    case Subsubsection_Begin() => "<h4>"
    case Subsubsection_End() => "</h4>"
    case Bf_Begin() => """<span style="font-weight:bold;">"""
    case Bf_End() => "<!--end bold--></span>"
    case It_Begin() => """<span style="font-style:italic;">"""
    case It_End() => "<!--end italic--></span>"
//    case Normalfont_Begin() => """<span style="font-family:serif;font-style:normal;font-weight:normal;">"""
//    case Normalfont_End() => "<!--end normal--></span>"
    case Large_Begin() => """<span style="font-size:x-large;">"""
    case Large_End() => "<!--end size--></span>"
  }
  
  def output(tokens: Seq[Token]): CharSequence =
  {
    val ret = for(token <- tokens) yield {// TODO use buffer
      token match {
        case White_Space(s) => s
        case Character(c) => character2string(c)
        case c: Command => command2string(c)
      }
    }
    ret.mkString
  }
  
  
  
  def apply(in: CharSequence): CharSequence =
  {
    Counter.reset_all()
    output(Lexer.parseAll(in))
  }
  
  
  
}


