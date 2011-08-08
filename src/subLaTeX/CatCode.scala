
package subLaTeX



import scala.collection.mutable._



object CatCode
{
  
  
  
  private val catcode = Map(
    '\\' -> Escape,
    '{' -> Begin,
    '}' -> End,
    '$' -> Math,
    '&' -> Align,
    '\n' -> Newline,
    '#' -> Param,
    '^' -> Super,
    '_' -> Sub,
    '\0' -> Ignore,
    ' ' -> Space,
    '\t' -> Space,
    '%' -> Comment,
    '\u007F' -> Invalid
  )
  
  
  
  def get(c: Char): CatCode =
  {
//    // Other
//    catcode.getOrElse(c, Other)
    catcode.get(c) match {
      case Some(cc) => cc
      // Other, Ignore
      case None => if(c.isControl) Ignore else Other
    }
  }
  
  def put(cc: CatCode, c: Char): Unit =
  {
    catcode.put(c, cc)
  }
  
  def put(cc: CatCode, cs: TraversableOnce[Char]): Unit =
  {
    for(c <- cs) put(cc, c)
  }
  
  def put(cc: CatCode, c1: Char, c2: Char, cs: Char*): Unit =
  {
    put(cc, c1)
    put(cc, c2)
    for(c <- cs) put(cc, c)
  }
  
  
  
  // Letter
  put(Letter, 'A' to 'Z')
  put(Letter, 'a' to 'z')
  
  // TODO "active"
  // TODO "invalid"
  
  
  
  
  
  
}



sealed abstract class CatCode
{
  def apply(c: Char): Boolean = CatCode.get(c) == this
}



object Escape extends CatCode
{
  override def toString = "escape"
}
object Begin extends CatCode
{
  override def toString = "begin"
}
object End extends CatCode
{
  override def toString = "end"
}
object Math extends CatCode
{
  override def toString = "math"
}
object Align extends CatCode
{
  override def toString = "align"
}
object Newline extends CatCode
{
  override def toString = "newline"
}
object Param extends CatCode
{
  override def toString = "param"
}
object Super extends CatCode
{
  override def toString = "super"
}
object Sub extends CatCode
{
  override def toString = "sub"
}
object Ignore extends CatCode
{
  override def toString = "ignore"
}
object Space extends CatCode
{
  override def toString = "space"
}
object Letter extends CatCode
{
  override def toString = "letter"
}
object Other extends CatCode
{
  override def toString = "other"
}
object Active extends CatCode
{
  override def toString = "active"
}
object Comment extends CatCode
{
  override def toString = "comment"
}
object Invalid extends CatCode
{
  override def toString = "invalid"
}


