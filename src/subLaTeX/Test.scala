
package subLaTeX



object Test// extends Lexer
{


//  val textInput = """
//\section{Special chars}  Eat this
//"""

  val textInput = """
\subsection{fonts and \{\}}

First try \bfseries bfseries in one paragraph

Does it leak to another ?? YES !!!

Does it go inside \{\}? {That's good question .. {YES !!!}}

Does it go through an environment ??
\begin{enumerate}\item first item\item second item\end{enumerate}
YES !!!

How do I turn it off ?? \normalfont Like this !!

{Does it leak out of a \bfseries group??} NO!!

Does it leak out of an environment??
\begin{enumerate}\item first item\item second \bfseries item\end{enumerate}
NO !!

{What will paragraph do \bfseries inside \par a group } together with some font ??



\section{Special chars ..}

Eat this .: \"a \~a \'a \^a \^{aa} (\~{ 	n \ldots\newline baobab }) \ldots    \ \ \ and nbsp-s\~\textbackslash

Backslash sometimes cancels newline
\
like this \ldots but that may be just nbsp

Reserved chars .: \# \$ \% \^{} \& \_ \{ \} \~{} \textbackslash

Not reserved chars .: @ ?? [ ] \par and some unicodes .: ∀ αβγ あ 日

Some quotes .: ``quotes'' ,,quotes'' "quotes" <<quotes>> `quotes' `'`'`; I will most probably ignore this \ldots

\section  *  {.. and numbers}

Number123number

Number1    23number

Number1  \\*  23number

Number1  \\  23number




Number1  \\\\\  23number\par123,567 123, 567 a,text a, text a  %commentComment!!!
    	new line
    	
123.567 123. 567 a.text a. text
"""


  def main(args: Array[String]): Unit =
  {
    println("Ahoj!!")


//    val lexer = Lexer.apply()
    println(Lexer.parseAll(Lexer.body, textInput))
//    println(parse(p, "ploda"))
    
    
    println("...caw")
  }

}
