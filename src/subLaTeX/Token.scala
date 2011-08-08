
package subLaTeX



/* TODO .:
 *  toString
 *  attrs like styling, etc..
 *  text level vs. block level
 */


sealed abstract case class Token

case class Command(content: String) extends Token
{
//  override def toString = "Command(" + content + ")"
}
case class White_Space(content: String) extends Token
{
  override def toString = "(" + content + ")"
}
case class Character(content: Char) extends Token
{
  override def toString = "" + content
}

case class Group(content: List[Token]) extends Token
{
  override def toString = "Group(" + content.mkString(", ") + ")"
}

case class ErrorToken(msg: String) extends Token
{
//  override def toString = "ErrorToken(" + msg + ")"
}

