
package subLaTeX



/* TODO .:
 *  toString
 *  attrs like styling, etc..
 *  text level vs. block level
 */


sealed abstract case class Token

case class Control(content: String) extends Token
{
//  override def toString = "Control(" + content + ")"
}
case class White_Space(content: String) extends Token
{
  override def toString = "WS(" + content + ")"
}
case class Character(content: Char) extends Token
{
  override def toString = "" + content
}

case class Group(content: List[Token]) extends Token
{
  override def toString = "Group(" + content.mkString(", ") + ")"
}


