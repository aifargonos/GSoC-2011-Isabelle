
package subLaTeX

/* TODO .:
 *  class Macro
 *  storage for macros
 *  stack for local def-s
 */

object Macro
{
  /* TODO
   *  storage for macros
   *    defining
   *    removing ??
   *  define basic macros !!!
   */
  
  def empty(command: String) = new Macro("", Lexer.success(()),// TODO !!! sensible default !!!
    {_ => new Compound(List(Simple(command))){override def toString = "Command(" + content.head.asInstanceOf[Simple].content + ")"}})
  val empty: Macro = empty("")

  val storage = scala.collection.mutable.Map.empty[String, Macro]

  def apply(name: String): Macro =
  {
//    storage(name)
    empty(name)// TODO .: debug
  }
  
  
}



class Macro(val name: String, after: Lexer.Parser[Any], code: Any => Block)
{
  /* TODO
   *  name (id, key in the storage)
   *  after parser
   *    its output should be an argument for code
   *  code
   *    should output a Block that is the final result of the parser command
   *
   *  copy(clone) for \let !!!
   */
   
  def apply(): Lexer.Parser[Block] =
  {
    after ^^ code
  }
  
  
  
}


