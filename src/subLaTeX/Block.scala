
package subLaTeX



import scala.collection.generic.Addable



/* TODO .:
 *  toString
 *  attrs like styling, etc..
 *  text level vs. block level
 */


sealed abstract case class Block

case class Simple(content: String) extends Block with Addable[Simple, Simple]
{
  override def repr = this
  override def +(elem: Simple): Simple = Simple(this.content + elem.content)
}

case class Compound(content: List[Block]) extends Block
{
  override def toString = "Compound(" + content.mkString(", ") + ")"
}


