
package subLaTeX



import scala.util.parsing.input.NoPosition
import scala.util.parsing.input.Position
import scala.util.parsing.input.Reader



class ListReader[T](list: Traversable[T], val end: T) extends Reader[T]
{
  
  private val (head, tail) = {
    if(list.isEmpty) {
      (end, list)
    } else {
      (list.head, list.tail)
    }
  }
  
  def first: T = head
  def rest: Reader[T] = new ListReader(tail, end)
  def pos: Position = NoPosition
  def atEnd: Boolean = {head == end}
  
}


