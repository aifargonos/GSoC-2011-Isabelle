
package subLaTeX



/* TODO .:
 *  toString
 *  attrs like styling, etc..
 *  text level vs. block level
 */


sealed abstract class Token

//case class Command(content: String) extends Token
//{
////  override def toString = "Command(" + content + ")"
//}
case class White_Space(content: String) extends Token
{
  override def toString = "(" + content + ")"
}
case class Character(content: Char) extends Token
{
  override def toString = "" + content
}

//case class Group(content: List[Token]) extends Token
//{
//  override def toString = "Group(" + content.mkString(", ") + ")"
//}
//
//case class ErrorToken(msg: String) extends Token
//{
////  override def toString = "ErrorToken(" + msg + ")"
//}

sealed abstract class Command extends Token
case class Unknown(command: String) extends Command
{
  override def toString = "Command(" + command + ")"
}
case class Par_Begin extends Command
case class Par_End extends Command

case class Line_Break extends Command
case class NBSP extends Command

case class Section_Begin extends Command
case class Section_End extends Command
case class Subsection_Begin extends Command
case class Subsection_End extends Command
case class Subsubsection_Begin extends Command
case class Subsubsection_End extends Command

case class Bf_Begin extends Command
case class Bf_End extends Command
case class It_Begin extends Command
case class It_End extends Command

case class Large_Begin extends Command
case class Large_End extends Command


