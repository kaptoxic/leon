package leon
package scife.domain

import purescala.Common._
import purescala.Trees._
import purescala.TypeTrees._

case class IntegerVal(val value: Int) extends Domain {
  
  override val tpe = Int32Type
  
  def constraint(v: Variable) =
    Equals(v, IntLiteral(value))

  def values = List(IntLiteral(value))
  
}