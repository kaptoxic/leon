package leon
package scife.domain

import purescala.Common._
import purescala.Trees._

case class Range(range: scala.Range, exclusive: Boolean = true) extends Domain[Int] {
  
  // inclusive range => exclusive flag
  assert(
    range.isInstanceOf[scala.Range.Inclusive] && !exclusive || exclusive
  )
  
  def constraint(v: Variable) =
    if(exclusive)
      And(        
        GreaterThan(v, IntLiteral(range.start)),
        LessThan(v, IntLiteral(range.end))
      )
    else
      throw new RuntimeException

  def values = range.toList

}