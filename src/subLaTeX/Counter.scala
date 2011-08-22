/*  Title:      src/subLaTeX/Counter.scala
    Author:     aifargonos

Counters for subLaTeX

as LaTeX counters.
*/

package subLaTeX



/* DONE .:
 *  newcounter by new Counter
 *  value by value
 *  setcounter by value_=
 *  stepcounter by step
 */

class Counter(val name: String)
{
  var value: Int = 0
  private val subcounters = scala.collection.mutable.Set.empty[Counter]

  Counter.storage.put(name, this)

  def this(name: String, supercounter: Counter) =
  {
    this(name)
    supercounter.subcounters += this
  }

  def this(name: String, supercounter: String) =
  {
    this(name, Counter.storage(supercounter))
  }

  def step() =
  {
    value += 1
    subcounters.foreach(_.value_=(0))
  }

}



object Counter
{
  
  private val storage = scala.collection.mutable.Map.empty[String, Counter]

  def apply(name: String) =
  {
    storage(name)
  }
  
  def reset_all() =
  {
    storage.foreach{p =>
      val (_, c) = p
      c.value_=(0)
    }
  }

  
  new Counter("chapter")
  new Counter("section", "chapter")
  new Counter("subsection", "section")
  new Counter("subsubsection", "subsection")
  
  
}


