
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
  
  
  
  def apply(c: Char) = get(c)
  
  
  
}



sealed abstract class CatCode
{
  
}



object Escape extends CatCode
{
  def apply(c: Char) = CatCode(c) == Escape
}
object Begin extends CatCode
{
  def apply(c: Char) = CatCode(c) == Begin
}
object End extends CatCode
{
  def apply(c: Char) = CatCode(c) == End
}
object Math extends CatCode
{
  def apply(c: Char) = CatCode(c) == Math
}
object Align extends CatCode
{
  def apply(c: Char) = CatCode(c) == Align
}
object Newline extends CatCode
{
  def apply(c: Char) = CatCode(c) == Newline
}
object Param extends CatCode
{
  def apply(c: Char) = CatCode(c) == Param
}
object Super extends CatCode
{
  def apply(c: Char) = CatCode(c) == Super
}
object Sub extends CatCode
{
  def apply(c: Char) = CatCode(c) == Sub
}
object Ignore extends CatCode
{
  def apply(c: Char) = CatCode(c) == Ignore
}
object Space extends CatCode
{
  def apply(c: Char) = CatCode(c) == Space
}
object Letter extends CatCode
{
  def apply(c: Char) = CatCode(c) == Letter
}
object Other extends CatCode
{
  def apply(c: Char) = CatCode(c) == Other
}
object Active extends CatCode
{
  def apply(c: Char) = CatCode(c) == Active
}
object Comment extends CatCode
{
  def apply(c: Char) = CatCode(c) == Comment
}
object Invalid extends CatCode
{
  def apply(c: Char) = CatCode(c) == Invalid
}


