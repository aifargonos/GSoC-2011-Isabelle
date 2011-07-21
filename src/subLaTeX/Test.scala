
package subLaTeX



object Test extends Lexer {


//  val textInput = """
//\section{Special chars}  Eat this
//"""

  val textInput = """
\section{Special chars ..}

Eat this .: \"a \~a \'a \^a \^{aa} \ldots    \ \ \ and nbsp-s

Backslash sometimes cancels newline
\
like this \ldots but that may be just nbsp

Reserved chars .: \# \$ \% \^{} \& \_ \{ \} \~{} \textbackslash

Not reserved chars .: @ ?? [ ]

Some quotes .: ``quotes'' ,,quotes'' "quotes" <<quotes>> `quotes' `'`'`; I will most probably ignore this \ldots

\section  *  {.. and numbers}

Number123number

Number1    23number

Number1  \\*  23number

Number1  \\  23number
"""


  def main(args: Array[String]): Unit =
  {
    println("Ahoj!!")

    
    println(parseAll(body, textInput))
//    println(parse(p, "ploda"))
    
    
    println("...caw")
  }

}
