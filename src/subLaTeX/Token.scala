/*  Title:      src/subLaTeX/Token.scala
    Author:     aifargonos

Output commands for subLaTeX


*/

package subLaTeX


/* TODO .:
 *  toString
 *  attrs like styling, etc..
 *  text level vs. block level ??
 */

sealed abstract class Token

case class White_Space(content: String) extends Token
{
  override def toString = "(" + content + ")"
}
case class Character(content: Char) extends Token
{
  override def toString = "" + content
}

sealed abstract class Command extends Token
case class Unknown(command: String) extends Command
{
  override def toString = "Command(" + command + ")"
}
case class Par_Begin() extends Command
case class Par_End() extends Command

case class Line_Break() extends Command
case class NBSP() extends Command

case class Header_Begin() extends Command
case class Header_End() extends Command
case class Chapter_Begin() extends Command
case class Chapter_End() extends Command
case class Section_Begin() extends Command
case class Section_End() extends Command
case class Subsection_Begin() extends Command
case class Subsection_End() extends Command
case class Subsubsection_Begin() extends Command
case class Subsubsection_End() extends Command

sealed abstract class Font_Command extends Command
case class Bf_Begin() extends Font_Command
case class Bf_End() extends Font_Command
case class It_Begin() extends Font_Command
case class It_End() extends Font_Command
//case class Normalfont_Begin extends Font_Command
//case class Normalfont_End extends Font_Command

sealed abstract class Font_Size_Command extends Command
case class Large_Begin() extends Font_Size_Command
case class Large_End() extends Font_Size_Command


