
package subLaTeX



import java.io.PrintStream
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.input.Reader

object Test// extends Lexer
{


//  val textInput = """
//\section{Special chars}  Eat this
//"""

  val textInput = """
\subsection{fonts and \{\}}

First try {\bfseries bfseries in one paragraph

Does it leak to another ?? YES !!!

Does it go inside \{\}? {That's good question .. {YES !!!}}

%Does it go through an environment ??
%\begin{enumerate}\item first item\item second item\end{enumerate}
%YES !!!

How do I turn it off ?? }Like this !!

{Does it leak out of a \bfseries group??} NO!!

%Does it leak out of an environment??
%\begin{enumerate}\item first item\item second \bfseries item\end{enumerate}
%NO !!

{What will paragraph do \bfseries inside \par a group } together with some font ??

{What will section do \itshape inside \subsection*{subsub\Large section} a group } together with some font ??

{What will textbf do \itshape inside \textbf{subsub\Large section} a group } together with some font ??

{\bfseries {{Normalfont \Large inside a }group} does this }% TODO .: this does not work yet !!!



\section{Special chars ..}

%Eat this .: \"a \~a \'a \^a \^{aa} (\~{ 	n \ldots\newline baobab }) \ldots    \ \ \ and nbsp-s\~\textbackslash

Backslash sometimes cancels newline
\
like this \ldots but that may be just nbsp

%Reserved chars .: \# \$ \% \^{} \& \_ \{ \} \~{} \textbackslash
Reserved chars .: \# \$ \%  \& \_ \{ \}  \textbackslash

Not reserved chars .: @ ?? [ ] \par and some unicodes .: ∀ αβγ あ 日

Some quotes .: ``quotes'' ,,quotes'' "quotes" <<quotes>> `quotes' `'`'`; I will most probably ignore this \ldots

\section  *  {.. and numbers}

Number123number

Number1    23number

Number1  \\*  23number

Number1  \\  23number

Number1  \newline  23number

Number1  \""" + "\n" + """  23number

Number1  \""" + "\t" + """  23number




Number1  \\\\\  23number\par123,567 123, 567 a,text a, text a  %commentComment!!!
    	new line
    	
123.567 123. 567 a.text a.	text \texttt{ttt}

\section \ldots
\section{three}
\subsection{four}
\subsection{five}
\subsubsection{six}
\subsection{seven}
\subsubsection{eight}
\section{nine}
\subsection{ten}
\subsection{eleven}
\subsection{twelve}
\section{thirteen}

\bfseries\ldots"""


  def main(args: Array[String]): Unit =
  {


////    val lexer = Lexer.apply()
////    println(Lexer.parseAll(Lexer.body, textInput))
//    println(Lexer.phrase(Lexer.body)(new CharSequenceReader(textInput)))
////    println(parse(p, "ploda"))
    
    println(Lexer.parseAll(new CharSequenceReader(textInput)))
//    println()
//    println(SubLaTeX(textInput))
    val out = new PrintStream("out.html")
    out.print("""<html>
<head>
<!--  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />-->
  <title>subLaTeX test</title>
</head>
<body>
""")
    out.print(new SubLaTeX()(textInput))
    out.print("""
</body>
</html>
""")
    
//    val scanner = new Lexer.Scanner(textInput)
//
//    def read_all(reader: Reader[Any]): Unit =
//    {
//      if(!reader.atEnd) {
//        print(reader.first + ", ")
//        read_all(reader.rest)
//      }
//    }
//
//    read_all(scanner)
//    println()
    
//    println(Parser.parseAll(scanner))

//    println("Letter: " + Letter)
//    val cc: CatCode = CatCode.get('\\')
//    println("Escape: " + cc.toString)

    
  }

}
